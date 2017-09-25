module Firkin
  ( View
  , ViewChange
  , Attr(..)
  , tap
  , text
  , element
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, newRef, readRef)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import DOM (DOM)
import DOM.Event.Event (Event)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument, htmlDocumentToNode, htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.Document (createElement, createTextNode)
import DOM.Node.Element (removeAttribute, setAttribute, setId)
import DOM.Node.Node (appendChild, lastChild, removeChild)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, Node, elementToEventTarget, elementToNode, textToNode)
import Data.Foldable (foldl, for_, sequence_)
import Data.Incremental (class ChangeStructure, Change, D1(D1), changeOf, diff, fromChange, lam, patch, runFunctionChange, toChange, valueOf)
import Data.Incremental.Eq (WrappedEq(..))
import Data.Incremental.Map (MapChange(..), MapChanges, WrappedMap)
import Data.Map (mapWithKey)
import Data.Maybe (Maybe(..))
import Data.Maybe.Last (Last(..))
import Data.Monoid (mempty)
import Data.Newtype (unwrap, wrap)

data Attr m
  = StringAttr String
  | Handler (Event -> Change m)

instance changeStructureAttr :: ChangeStructure (Attr dm) (Last (Attr dm)) where
  diff (StringAttr s1) (StringAttr s2) | s1 == s2 = mempty
  diff _ new = Last (Just new)
  patch old (Last Nothing) = old
  patch _   (Last (Just new)) = new

type ElementData m =
  { element  :: String
  , attrs    :: WrappedMap String (Attr m)
  , children :: WrappedMap String (View m)
  }

data View m
  = Text String
  | Element (ElementData m)

data ViewChange m
  = Replace (View m)
  | EditAttrs (MapChanges String (Attr m) (Last (Attr m)))
  | EditChildren (MapChanges String (View m) (Array (ViewChange m)))

instance changeStructureView :: ChangeStructure (View model) (Array (ViewChange model)) where
  diff (Element e1) (Element e2) | e1.element == e2.element =
    [ EditAttrs (diff e1.attrs e2.attrs)
    , EditChildren (diff e1.children e2.children)
    ]
  diff _ new = [Replace new]
  patch = foldl patchOne where
    patchOne _ (Replace new) = new
    patchOne (Element e) (EditAttrs m) = Element e { attrs = patch e.attrs m }
    patchOne (Element e) (EditChildren m) = Element e { children = patch e.children m }
    patchOne old _ = old

text :: forall model. D1 (WrappedEq String -> View model)
text = lam \(D1 (WrappedEq s) c) -> D1 (Text s) (toChange (update (fromChange c))) where
  update :: Last String -> Array (ViewChange model)
  update (Last Nothing) = []
  update (Last (Just new)) = [Replace (Text new)]

element
  :: forall model
   . String
  -> D1 (WrappedMap String (Attr model)
          -> WrappedMap String (View model)
          -> View model)
element elName =
    lam \(D1 attrs d_attrs) ->
      lam \(D1 children d_children) ->
        D1 (Element { element: elName, attrs, children })
           (toChange (collectChanges (fromChange d_attrs) (fromChange d_children)))
  where
    collectChanges attrChanges childrenChanges =
      [ EditAttrs attrChanges
      , EditChildren childrenChanges
      ]

tap
  :: forall model d_model eff
   . ChangeStructure model d_model
  => Element
  -> D1 (model -> View model)
  -> model
  -> Eff (dom :: DOM, ref :: REF | eff) Unit
tap root f initialModel = do
  let initialView = valueOf f initialModel
      toPatch = runFunctionChange (fromChange (changeOf f))
  tapWith root initialModel (\m c -> toPatch m (fromChange c)) initialView

tapWith
  :: forall model eff
   . Element
  -> model
  -> (model -> Change model -> Array (ViewChange model))
  -> View model
  -> Eff (dom :: DOM, ref :: REF | eff) Unit
tapWith root model updater v = do
  modelRef <- newRef model
  document <- window >>= document
  let go :: Node -> Maybe String -> View model -> Eff (dom :: DOM, ref :: REF | eff) Unit
      go n id_ (Text s) = void do
        ne <- createElement "span" (htmlDocumentToDocument document)
        for_ id_ \idVal ->
          setId (wrap idVal) ne
        tn <- createTextNode s (htmlDocumentToDocument document)
        _ <- appendChild (textToNode tn) (elementToNode ne)
        appendChild (elementToNode ne) n
      go n id_ (Element e) = void (addElement n id_ e)

      addElement :: Node -> Maybe String -> ElementData model -> Eff (dom :: DOM, ref :: REF | eff) Element
      addElement n id_ { attrs, element: el, children } = do
        ne <- createElement el (htmlDocumentToDocument document)
        for_ id_ \idVal ->
          setId (wrap idVal) ne
        sequence_ (mapWithKey (setAttr ne) (unwrap attrs))
        sequence_ (mapWithKey (\k -> go (elementToNode ne) (Just k)) (unwrap children))
        _ <- appendChild (elementToNode ne) n
        pure ne

      setAttr :: Element -> String -> Attr model -> Eff (dom :: DOM, ref :: REF | eff) Unit
      setAttr e k (StringAttr s) =
        setAttribute k s e
      setAttr e k (Handler f) = do
        let handler ev = do
              currentModel <- readRef modelRef
              let patches = updater currentModel (f ev)
              for_ patches (applyPatch root)
        addEventListener (wrap k) (eventListener handler) false (elementToEventTarget e)

      removeAllChildren :: Node -> Eff (dom :: DOM, ref :: REF | eff) Unit
      removeAllChildren n = tailRecM loop unit where
        loop _ = do
          child <- lastChild n
          case child of
            Nothing -> pure (Done unit)
            Just child_ -> do _ <- removeChild child_ n
                              pure (Loop unit)

      applyPatch :: Element -> ViewChange model -> Eff (dom :: DOM, ref :: REF | eff) Unit
      applyPatch e (Replace new) = do
        removeAllChildren (elementToNode e)
        go (elementToNode e) Nothing new
      applyPatch e (EditAttrs attrs) = sequence_ (mapWithKey applyAttr (unwrap attrs)) where
        applyAttr :: String
                  -> MapChange (Attr model) (Last (Attr model))
                  -> Eff (dom :: DOM, ref :: REF | eff) Unit
        applyAttr k (Add val) = setAttr e k val
        applyAttr k Remove = removeAttribute k e
        applyAttr k (Update u) = for_ (unwrap u) (setAttr e k)
      applyPatch n (EditChildren children) = do
        let updateChild k (Add view) = go (elementToNode n) (Just k) view
            updateChild k Remove = do
              child <- getElementById (wrap k) (htmlDocumentToNonElementParentNode document)
              for_ child \child_ -> removeChild (elementToNode n) (htmlDocumentToNode document)
            updateChild k (Update dv) = do
              child <- getElementById (wrap k) (htmlDocumentToNonElementParentNode document)
              for_ dv \dv_ ->
                for_ child \child_ ->
                  applyPatch child_ dv_
        sequence_ (mapWithKey updateChild (unwrap children))
  go (elementToNode root) Nothing v
