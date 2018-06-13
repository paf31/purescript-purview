module Purview
  ( View
  , ViewChanges
  , text
  , textWith
  , element
  , element_
  , render
  , Component
  , run
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Incremental (class Patch, Change, Jet, constant, fromChange, patch, toChange)
import Data.Incremental.Array (ArrayChange(..), IArray)
import Data.Incremental.Eq (Atomic)
import Data.Incremental.Map (MapChange(..), MapChanges, IMap)
import Data.Map (empty)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe.Last (Last)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (for)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref (new, read, write) as Ref
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)
import Web.DOM.Document (createElement, createTextNode)
import Web.DOM.Element (Element)
import Web.DOM.Element as Element
import Web.DOM.Node (Node)
import Web.DOM.Node as Node
import Web.DOM.NodeList as NodeList
import Web.DOM.Text as Text
import Web.Event.EventTarget (EventListener, addEventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
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
  , handlers :: IMap String (Atomic (Effect EventListener))
  , kids     :: IArray View
  }

-- | The (abstract) type of view updates.
-- |
-- | `View`s can be applied to the DOM using the `applyPatch` function.
newtype ViewChanges = ViewChanges
  { text     :: Last String
  , attrs    :: MapChanges String (Atomic String) (Last String)
  , handlers :: MapChanges String (Atomic (Effect EventListener)) (Last (Effect EventListener))
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

view_
  :: String
  -> Jet (Atomic String)
  -> Jet (IMap String (Atomic String))
  -> Jet (IMap String (Atomic (Effect EventListener)))
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
  -> Jet (IMap String (Atomic (Effect EventListener)))
  -> Jet (IArray View)
  -> Jet View
element elName = view_ elName (constant (wrap ""))

-- | Create an element with no attributes or event handlers.
element_
  :: String
  -> Jet (IArray View)
  -> Jet View
element_ elName kids = view_ elName (constant (wrap "")) (constant (wrap empty)) (constant (wrap empty)) kids

-- | Compile a View to DOM elements, returning the root element and an
-- | effectful patching function.
render
  ::  View
  ->  Effect (Tuple Element (ViewChanges -> Effect Unit))
render (View v) = do
  doc <- HTMLDocument.toDocument <$> (window >>= document)
  root <- createElement v.element doc
  txt <- createTextNode (unwrap v.text) doc
  _ <- Node.appendChild (Text.toNode txt) (Element.toNode root)
  forWithIndex_ (unwrap v.attrs) \k s ->
    Element.setAttribute k (unwrap s) root
  listeners <- forWithIndex (unwrap v.handlers) \k mh -> do
    h <- unwrap mh
    addEventListener (wrap k) h false (Element.toEventTarget root)
    pure h
  childRenders <- for (unwrap v.kids) render
  for_ childRenders \(Tuple ele _) ->
    Node.appendChild (Element.toNode ele) (Element.toNode root)
  listenersRef <- (Ref.new <<< Object.fromFoldable)
    (Map.toUnfoldableUnordered listeners :: Array (Tuple String EventListener))
  patchersRef <- Ref.new (map Tuple.snd childRenders)
  pure (Tuple root (applyPatch root listenersRef patchersRef))

-- TODO: these obj* functions could be extracted to an independent package
-- about mutable Objects.

foreign import objDelete :: forall a. EffectFn2 String (Ref (Object a)) Unit

foreign import objUpsert :: forall a. EffectFn3 String a (Ref (Object a)) Unit

foreign import objUpdate :: forall a.
  EffectFn3 String (a -> a) (Ref (Object a)) Unit

-- | Apply a View patch to a DOM element, also given a reference to the
-- | element's current event listeners and child patching functions.
-- |
-- | Internal use only.
applyPatch ::
      Element
  ->  Ref (Object EventListener)
  -- TODO: An Array will not be efficient at large size for insert and
  -- delete operations.
  ->  Ref (Array (ViewChanges -> Effect Unit))
  ->  ViewChanges
  ->  Effect Unit
applyPatch root listeners patchers (ViewChanges vc) = do

  _ <- for_ vc.text (_ `Node.setTextContent` (Element.toNode root))
  forWithIndex_ (unwrap vc.attrs) updateAttr
  forWithIndex_ (unwrap vc.handlers) updateHandler
  for_ vc.kids updateChildren

  where

  updateAttr
    :: String
    -> MapChange (Atomic String) (Last String)
    -> Effect Unit
  updateAttr k (Add val) = Element.setAttribute k (unwrap val) root
  updateAttr k Remove = Element.removeAttribute k root
  updateAttr k (Update u) = for_ (unwrap u) \s ->
    Element.setAttribute k s root

  updateHandler
    :: String
    -> MapChange (Atomic (Effect EventListener)) (Last (Effect EventListener))
    -> Effect Unit
  updateHandler k (Add mh) = do
    h <- unwrap mh
    addEventListener (wrap k) h false (Element.toEventTarget root)
    runEffectFn3 objUpsert k h listeners
  updateHandler k Remove = do
    ls <- Ref.read listeners
    case Object.lookup k ls of
      Nothing -> unsafeCrashWith $
        "Purview lost track of a " <> show k <> " EventListener during a remove"
      Just h -> do
        removeEventListener (wrap k) h false (Element.toEventTarget root)
        runEffectFn2 objDelete k listeners
  updateHandler k (Update dh) = do
    case unwrap dh of
      Nothing -> pure unit
      Just mh -> do
        ls <- Ref.read listeners
        case Object.lookup k ls of
          Nothing -> unsafeCrashWith $ "Purview lost track of a " <> show k
            <> "EventListener during an update"
          Just h -> do
            h' <- mh
            removeEventListener (wrap k) h false (Element.toEventTarget root)
            addEventListener (wrap k) h' false (Element.toEventTarget root)
            runEffectFn3 objUpsert k h' listeners

  updateChildren
    :: ArrayChange View ViewChanges
    -> Effect Unit
  updateChildren (InsertAt i vw) = do
    Tuple ele patcher <- render vw
    mn <- Node.childNodes (Element.toNode root) >>= NodeList.item i
    case mn of
      Nothing ->
        Node.appendChild (Element.toNode ele) (Element.toNode root) $> unit
      Just n ->
        Node.insertBefore (Element.toNode ele) n (Element.toNode root) $> unit
    ps <- Ref.read patchers
    case Array.insertAt i patcher ps of
      Nothing -> Ref.write (Array.snoc ps patcher) patchers
      Just patchers' -> Ref.write patchers' patchers
  updateChildren (DeleteAt i) = do
    mc <- Node.childNodes (Element.toNode root) >>= NodeList.item i
    case mc of
      Nothing -> unsafeCrashWith $
        "Purview lost track of child #" <> show i <> " during a delete"
      Just c -> do
        _ <- Node.removeChild c (Element.toNode root)
        ps <- Ref.read patchers
        case Array.deleteAt i ps of
          Nothing -> unsafeCrashWith $
            "Purview lost track of the patcher for child #" <> show i
            <> " during a delete"
          Just patchers' -> Ref.write patchers' patchers
  updateChildren (ModifyAt i dv) = do
    mc <- Node.childNodes (Element.toNode root) >>= NodeList.item i
    case mc of
      Nothing -> unsafeCrashWith $
        "Purview lost track of child #" <> show i <> " during a modify"
      Just c -> do
        ps <- Ref.read patchers
        case Array.index ps i of
          Nothing -> unsafeCrashWith $
            "Purview lost track of the patcher for child #" <> show i
            <> " during a modify"
          Just patcher -> patcher dv

-- | An example component type, used by the `run` function.
-- |
-- | A component takes a changing update function, and a changing `model`
-- | and returns a changing `View`. The update function receives a `Change` to
-- | the model and applies it.
type Component model
   = Jet (Atomic (Change model -> Effect Unit))
  -> Jet model
  -> Jet View

-- | Install and run a component as a child of the given node.
run
  :: forall model change
   . Patch model change
  => Node
  -> Component model
  -> model
  -> Effect Unit
run parent component initialModel = do
  modelRef <- Ref.new initialModel
  patcherRef <- Ref.new (\_ -> unsafeCrashWith "Purview has no patcher to use")
  let onChange modelChange = do
        patcher <- Ref.read patcherRef
        model <- Ref.read modelRef
        Ref.write (patch model (fromChange modelChange)) modelRef
        patcher (fromChange (component (constant (wrap onChange))
          { position: model, velocity: modelChange }).velocity)
  let initialView = component (constant (wrap onChange)) (constant initialModel)
  Tuple root patcher <- render initialView.position
  Ref.write patcher patcherRef
  _ <- Node.appendChild (Element.toNode root) parent
  pure unit
