module Purview
  ( View
  , ViewChanges
  , text
  , textWith
  , element
  , element_
  , unsafeEventListener
  , render
  , Component
  , run
  ) where

import Prelude

import Data.Array ((!!))
import Data.Foldable (foldM, sequence_, traverse_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Incremental (class Patch, Change, Jet, constant, fromChange, patch, toChange)
import Data.Incremental.Array (ArrayChange(..), IArray)
import Data.Incremental.Eq (Atomic)
import Data.Incremental.Map (MapChange(..), MapChanges, IMap)
import Data.Map (empty, lookup)
import Data.Maybe (Maybe(..))
import Data.Maybe.Last (Last)
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Effect.Ref (new, read, write) as Ref
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)
import Web.DOM.Document (createDocumentFragment, createElement, createTextNode)
import Web.DOM.DocumentFragment as DocumentFragment
import Web.DOM.Node (Node)
import Web.DOM.Element (Element, removeAttribute, setAttribute)
import Web.DOM.Element as Element
import Web.DOM.HTMLCollection (item)
import Web.DOM.Node as Node
import Web.DOM.ParentNode (children, firstElementChild)
import Web.DOM.Text as Text
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventListener, addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

-- | The (abstract) type of views.
-- |
-- | You can create (incremental) functions returning values of this type by
-- | using the `text` and `element` functions.
-- |
-- | `View`s can be initially rendered to the DOM using the `render` function.
newtype View = View
  { element  :: String
  , text     :: Atomic String
  , attrs    :: IMap String (Atomic String)
  , handlers :: IMap String (Atomic EventListener)
  , kids     :: IArray View
  }

-- | The (abstract) type of view updates.
-- |
-- | `View`s can be applied to the DOM using the `applyPatch` function.
newtype ViewChanges = ViewChanges
  { text     :: Last String
  , attrs    :: MapChanges String (Atomic String) (Last String)
  , handlers :: MapChanges String (Atomic EventListener) (Last EventListener)
  , kids     :: Array (ArrayChange View ViewChanges)
  }

instance semigroupViewChanges :: Semigroup ViewChanges where
  append (ViewChanges a) (ViewChanges b) =
    ViewChanges
      { text:     a.text     <> b.text
      , attrs:    a.attrs    <> b.attrs
      , handlers: a.handlers <> b.handlers
      , kids:     a.kids     <> b.kids
      }

instance monoidViewChanges :: Monoid ViewChanges where
  mempty = ViewChanges { text: mempty, attrs: mempty, handlers: mempty, kids: mempty }

instance patchView :: Patch View ViewChanges where
  patch (View v) (ViewChanges vc) =
    View v { text     = patch v.text     vc.text
           , attrs    = patch v.attrs    vc.attrs
           , handlers = patch v.handlers vc.handlers
           , kids     = patch v.kids     vc.kids
           }

-- | Create an `EventListener` unsafely.
-- |
-- | This operation is unsafe in the sense that applying it multiple times
-- | to the same argument will result in event listeners which are not
-- | substitutable in all contexts.
-- |
-- | However, for the purposes of the current API, it's very useful to be
-- | able to create `EventListener`s unsafely in this way, and then store
-- | the results in a `View`.
unsafeEventListener :: (Event -> Effect Unit) -> EventListener
unsafeEventListener = unsafePerformEffect <<< eventListener

view_
  :: String
  -> Jet (Atomic String)
  -> Jet (IMap String (Atomic String))
  -> Jet (IMap String (Atomic EventListener))
  -> Jet (IArray View)
  -> Jet View
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
textWith :: String -> Jet (Atomic String) -> Jet View
textWith elName s = view_ elName s (constant (wrap empty)) (constant (wrap empty)) (constant (wrap []))

-- | Create a text node wrapped in a `<span>` element.
text :: Jet (Atomic String) -> Jet View
text = textWith "span"

-- | Create an element with the given name, attributes, event listeners and
-- | children.
element
  :: String
  -> Jet (IMap String (Atomic String))
  -> Jet (IMap String (Atomic EventListener))
  -> Jet (IArray View)
  -> Jet View
element elName = view_ elName (constant (wrap ""))

-- | Create an element with no attributes or event handlers.
element_
  :: String
  -> Jet (IArray View)
  -> Jet View
element_ elName kids = view_ elName (constant (wrap "")) (constant (wrap empty)) (constant (wrap empty)) kids

-- | Render a `View` to the DOM, under the given `Node`, and connect any
-- | event listeners.
-- |
-- | Once the initial `View` is rendered, the DOM can be updated using the
-- | `applyPatch` function.
render :: Node -> View -> Effect Unit
render n (View v) = do
  doc <- window >>= document >>> map toDocument
  ne <- createElement v.element doc
  tn <- createTextNode (unwrap v.text) doc
  _ <- Node.appendChild (Text.toNode tn) (Element.toNode ne)
  sequence_ (mapWithIndex (\k s -> setAttribute k (unwrap s) ne) (unwrap v.attrs))
  sequence_ (mapWithIndex (\k h -> addEventListener (wrap k) (unwrap h) false (Element.toEventTarget ne)) (unwrap v.handlers))
  traverse_ (render (Element.toNode ne)) (unwrap v.kids)
  _ <- Node.appendChild (Element.toNode ne) n
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
applyPatch :: Element -> View -> ViewChanges -> Effect Unit
applyPatch e vv@(View v) (ViewChanges vc) = do
    _ <- traverse_ (_ `Node.setTextContent` Element.toNode e) vc.text
    sequence_ (mapWithIndex updateAttr (unwrap vc.attrs))
    sequence_ (mapWithIndex updateHandler (unwrap vc.handlers))
    void $ foldM updateChildren v.kids vc.kids
  where
    updateAttr
      :: String
      -> MapChange (Atomic String) (Last String)
      -> Effect Unit
    updateAttr k (Add val) = setAttribute k (unwrap val) e
    updateAttr k Remove = removeAttribute k e
    updateAttr k (Update u) = traverse_ (\s -> setAttribute k s e) (unwrap u)

    updateHandler
      :: String
      -> MapChange (Atomic EventListener) (Last EventListener)
      -> Effect Unit
    updateHandler k (Add h) = do
      addEventListener (wrap k) (unwrap h) false (Element.toEventTarget e)
    updateHandler k Remove = do
      lookup k (unwrap v.handlers) # traverse_ \h ->
        removeEventListener (wrap k) (unwrap h) false (Element.toEventTarget e)
    updateHandler k (Update dh) = dh # traverse_ \new -> do
      lookup k (unwrap v.handlers) # traverse_ \old ->
        removeEventListener (wrap k) (unwrap old) false (Element.toEventTarget e)
      addEventListener (wrap k) new false (Element.toEventTarget e)

    updateChildren
      :: IArray View
      -> ArrayChange View ViewChanges
      -> Effect (IArray View)
    updateChildren kids ch@(InsertAt i vw) = do
      doc <- window >>= document >>> map toDocument
      cs <- children (Element.toParentNode e)
      mc <- item i cs
      newNode <- DocumentFragment.toNode <$> createDocumentFragment doc
      render newNode vw
      _ <- case mc of
        Just c -> Node.insertBefore newNode (Element.toNode c) (Element.toNode e)
        Nothing -> Node.appendChild newNode (Element.toNode e)
      pure (patch kids [ch])
    updateChildren kids ch@(DeleteAt i) = do
      cs <- children (Element.toParentNode e)
      mc <- item i cs
      case mc of
        Just c -> void (Node.removeChild (Element.toNode c) (Element.toNode e))
        Nothing -> pure unit
      pure (patch kids [ch])
    updateChildren kids ch@(ModifyAt i dv) = do
      cs <- children (Element.toParentNode e)
      mc <- item i cs
      mc # traverse_ \c ->
        unwrap kids !! i # traverse_ \cv ->
          applyPatch c cv dv
      pure (patch kids [ch])

-- | An example component type, used by the `run` function.
-- |
-- | A component takes a changing update function, and a changing `model`
-- | and returns a changing `View`. The update function receives a `Change` to
-- | the model and applies it.
type Component model
   = Jet (Atomic (Change model -> Effect Unit))
  -> Jet model
  -> Jet View

-- | An example implementation of an application loop.
-- |
-- | Renders a `View` to the DOM under the given `Node`. The `View` can depend
-- | on the current value of the `model`, which can change over time by the
-- | application of `Change`s in event handlers.
run
  :: forall model change
   . Patch model change
  => Element
  -> Component model
  -> model
  -> Effect Unit
run root component initialModel = do
  modelRef <- Ref.new initialModel
  viewRef <- Ref.new Nothing
  document <- window >>= document
  let initialView = (component (constant (wrap onChange)) (constant initialModel)).position
      onChange modelChange = do
        currentModel <- Ref.read modelRef
        currentView_ <- Ref.read viewRef
        case currentView_ of
          Nothing -> unsafeCrashWith "viewRef was empty"
          Just currentView -> do
            let newModel = patch currentModel (fromChange modelChange)
                patches = updater currentModel modelChange
                -- Compute and store the new view based on the patch we are about
                -- to apply. This way, we can use the stored view to detach event
                -- handlers correctly later, if necessary.
                newView = patch currentView patches
            Ref.write newModel modelRef
            Ref.write (Just newView) viewRef
            firstElementChild (Element.toParentNode root) >>= traverse_ \e ->
              applyPatch e currentView patches
      updater m dm = fromChange (component (constant (wrap onChange)) { position: m, velocity: dm }).velocity
  Ref.write (Just initialView) viewRef
  render (Element.toNode root) initialView
