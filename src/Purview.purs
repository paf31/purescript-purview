module Purview
  ( View
  , ViewChanges
  , text
  , textWith
  , element
  , element_
  , render
  , applyPatch
  , run
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import DOM (DOM)
import DOM.Event.EventTarget (EventListener, addEventListener, removeEventListener)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Document (createDocumentFragment, createElement, createTextNode)
import DOM.Node.Element (removeAttribute, setAttribute)
import DOM.Node.HTMLCollection (item)
import DOM.Node.Node (appendChild, insertBefore, removeChild, setTextContent)
import DOM.Node.ParentNode (children, firstElementChild)
import DOM.Node.Types (Element, Node, documentFragmentToNode, elementToEventTarget, elementToNode, elementToParentNode, textToNode)
import Data.Array ((!!))
import Data.Foldable (sequence_, traverse_)
import Data.Incremental (class Patch, Change, Jet, constant, fromChange, patch, toChange)
import Data.Incremental.Array (ArrayChange(..), IArray)
import Data.Incremental.Eq (Atomic)
import Data.Incremental.Map (MapChange(..), MapChanges, IMap)
import Data.Map (empty, lookup, mapWithKey)
import Data.Maybe (Maybe(..))
import Data.Maybe.Last (Last)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (unwrap, wrap)
import Partial.Unsafe (unsafeCrashWith)

-- | The (abstract) type of views.
-- |
-- | You can create (incremental) functions returning values of this type by
-- | using the `text` and `element` functions.
-- |
-- | `View`s can be initially rendered to the DOM using the `render` function.
newtype View eff = View
  { element  :: String
  , text     :: Atomic String
  , attrs    :: IMap String (Atomic String)
  , handlers :: IMap String (Atomic (EventListener eff))
  , kids     :: IArray (View eff)
  }

-- | The (abstract) type of view updates.
-- |
-- | `View`s can be applied to the DOM using the `applyPatch` function.
newtype ViewChanges eff = ViewChanges
  { text     :: Last String
  , attrs    :: MapChanges String (Atomic String) (Last String)
  , handlers :: MapChanges String (Atomic (EventListener eff)) (Last (EventListener eff))
  , kids     :: Array (ArrayChange (View eff) (ViewChanges eff))
  }

instance semigroupViewChanges :: Semigroup (ViewChanges eff) where
  append (ViewChanges a) (ViewChanges b) =
    ViewChanges
      { text:     a.text     <> b.text
      , attrs:    a.attrs    <> b.attrs
      , handlers: a.handlers <> b.handlers
      , kids:     a.kids     <> b.kids
      }

instance monoidViewChanges :: Monoid (ViewChanges eff) where
  mempty = ViewChanges { text: mempty, attrs: mempty, handlers: mempty, kids: mempty }

instance patchView :: Patch (View eff) (ViewChanges eff) where
  patch (View v) (ViewChanges vc) =
    View v { text     = patch v.text     vc.text
           , attrs    = patch v.attrs    vc.attrs
           , handlers = patch v.handlers vc.handlers
           , kids     = patch v.kids     vc.kids
           }

view_
  :: forall eff
   . String
  -> Jet (Atomic String)
  -> Jet (IMap String (Atomic String))
  -> Jet (IMap String (Atomic (EventListener eff)))
  -> Jet (IArray (View eff))
  -> Jet (View eff)
view_ elName text_ attrs handlers kids =
  { position: View
      { element: elName
      , text: text_.position
      , attrs: attrs.position
      , handlers: handlers.position
      , kids: kids.position
      }
  , velocity: toChange $ ViewChanges
      { text: fromChange text_.velocity
      , attrs: fromChange attrs.velocity
      , handlers: fromChange handlers.velocity
      , kids: fromChange kids.velocity
      }
  }

-- | Create a text node wrapped in an element with the specified name.
textWith :: forall eff. String -> Jet (Atomic String) -> Jet (View eff)
textWith elName s = view_ elName s (constant (wrap empty)) (constant (wrap empty)) (constant (wrap []))

-- | Create a text node wrapped in a `<span>` element.
text :: forall eff. Jet (Atomic String) -> Jet (View eff)
text = textWith "span"

-- | Create an element with the given name, attributes, event listeners and
-- | children.
element
  :: forall eff
   . String
  -> Jet (IMap String (Atomic String))
  -> Jet (IMap String (Atomic (EventListener eff)))
  -> Jet (IArray (View eff))
  -> Jet (View eff)
element elName = view_ elName (constant (wrap ""))

-- | Create an element with no attributes or event handlers.
element_
  :: forall eff
   . String
  -> Jet (IArray (View eff))
  -> Jet (View eff)
element_ elName kids = view_ elName (constant (wrap "")) (constant (wrap empty)) (constant (wrap empty)) kids

-- | Render a `View` to the DOM, under the given `Node`, and connect any
-- | event listeners.
-- |
-- | Once the initial `View` is rendered, the DOM can be updated using the
-- | `applyPatch` function.
render
  :: forall eff
   . Node
  -> View (dom :: DOM | eff)
  -> Eff (dom :: DOM | eff) Unit
render n (View v) = do
  doc <- window >>= document
  ne <- createElement v.element (htmlDocumentToDocument doc)
  tn <- createTextNode (unwrap v.text) (htmlDocumentToDocument doc)
  _ <- appendChild (textToNode tn) (elementToNode ne)
  sequence_ (mapWithKey (\k s -> setAttribute k (unwrap s) ne) (unwrap v.attrs))
  sequence_ (mapWithKey (\k h -> addEventListener (wrap k) (unwrap h) false (elementToEventTarget ne)) (unwrap v.handlers))
  traverse_ (render (elementToNode ne)) (unwrap v.kids)
  _ <- appendChild (elementToNode ne) n
  pure unit

-- | Apply a set of `ViewChanges` to the DOM, under the given `Node`, which should
-- | be the same as the one initially passed to `render`.
-- |
-- | The second argument is the _most-recently rendered_ `View`, i.e. the one which
-- | should correspond to the current state of the DOM.
-- |
-- | _Note_: in order to correctly remove event listeners, the `View` passed in
-- | must contain the same event listeners as those last attached, _by reference_.
-- | In practice, this means that the `View` passed into this function should be
-- | obtained using the `patch` function.
-- |
-- | See the implementation of the `run` function for an example.
applyPatch
  :: forall eff
   . Element
  -> View (dom :: DOM | eff)
  -> ViewChanges (dom :: DOM | eff)
  -> Eff (dom :: DOM | eff) Unit
applyPatch e (View v) (ViewChanges vc) = do
    _ <- traverse_ (_ `setTextContent` (elementToNode e)) vc.text
    sequence_ (mapWithKey updateAttr (unwrap vc.attrs))
    sequence_ (mapWithKey updateHandler (unwrap vc.handlers))
    traverse_ updateChildren vc.kids
  where
    updateAttr
      :: String
      -> MapChange (Atomic String) (Last String)
      -> Eff (dom :: DOM | eff) Unit
    updateAttr k (Add val) = setAttribute k (unwrap val) e
    updateAttr k Remove = removeAttribute k e
    updateAttr k (Update u) = traverse_ (\s -> setAttribute k s e) (unwrap u)

    updateHandler
      :: String
      -> MapChange (Atomic (EventListener (dom :: DOM | eff))) (Last (EventListener (dom :: DOM | eff)))
      -> Eff (dom :: DOM | eff) Unit
    updateHandler k (Add h)    = do
      addEventListener (wrap k) (unwrap h) false (elementToEventTarget e)
    updateHandler k Remove     = do
      lookup k (unwrap v.handlers) # traverse_ \h ->
        removeEventListener (wrap k) (unwrap h) false (elementToEventTarget e)
    updateHandler k (Update dh) = dh # traverse_ \new -> do
      lookup k (unwrap v.handlers) # traverse_ \old ->
        removeEventListener (wrap k) (unwrap old) false (elementToEventTarget e)
      addEventListener (wrap k) new false (elementToEventTarget e)

    updateChildren
      :: ArrayChange (View (dom :: DOM | eff)) (ViewChanges (dom :: DOM | eff))
      -> Eff (dom :: DOM | eff) Unit
    updateChildren (InsertAt i vw) = void do
      doc <- window >>= document
      cs <- children (elementToParentNode e)
      mc <- item i cs
      newNode <- documentFragmentToNode <$> createDocumentFragment (htmlDocumentToDocument doc)
      render newNode vw
      case mc of
        Just c -> insertBefore newNode (elementToNode c) (elementToNode e)
        Nothing -> appendChild newNode (elementToNode e)
    updateChildren (DeleteAt i) = do
      cs <- children (elementToParentNode e)
      mc <- item i cs
      case mc of
        Just c -> void (removeChild (elementToNode c) (elementToNode e))
        Nothing -> pure unit
    updateChildren (ModifyAt i dv) = do
      cs <- children (elementToParentNode e)
      mc <- item i cs
      mc # traverse_ \c ->
        unwrap v.kids !! i # traverse_ \cv ->
          applyPatch c cv dv

-- | An example implementation of an application loop.
-- |
-- | Renders a `View` to the DOM under the given `Node`. The `View` can depend
-- | on the current value of the `model`, which can change over time by the
-- | application of `Change`s in event handlers.
run
  :: forall model change eff
   . Patch model change
  => Element
  -> ((Change model -> Eff (dom :: DOM, ref :: REF | eff) Unit) -> Jet model -> Jet (View (dom :: DOM, ref :: REF | eff)))
  -> model
  -> Eff (dom :: DOM, ref :: REF | eff) Unit
run root view initialModel = do
  modelRef <- newRef initialModel
  viewRef <- newRef Nothing
  document <- window >>= document
  let initialView = (view onChange (constant initialModel)).position
      onChange modelChange = do
        currentModel <- readRef modelRef
        currentView_ <- readRef viewRef
        case currentView_ of
          Nothing -> unsafeCrashWith "viewRef was empty"
          Just currentView -> do
            let newModel = patch currentModel (fromChange modelChange)
                patches = updater currentModel modelChange
                -- Compute and store the new view based on the patch we are about
                -- to apply. This way, we can use the stored view to detach event
                -- handlers correctly later, if necessary.
                newView = patch currentView patches
            writeRef modelRef newModel
            writeRef viewRef (Just newView)
            firstElementChild (elementToParentNode root) >>= traverse_ \e ->
              applyPatch e currentView patches
      updater m dm = fromChange (view onChange { position: m, velocity: dm }).velocity
  writeRef viewRef (Just initialView)
  render (elementToNode root) initialView
