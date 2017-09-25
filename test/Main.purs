module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import Data.Incremental (class ChangeStructure, D1(D1), app, constant, diff, fromChange, lam, toChange)
import Data.Incremental.Eq (WrappedEq)
import Data.Incremental.Map (MapChange(Update), WrappedMap)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe.Last (Last(..))
import Data.Newtype (unwrap, wrap)
import Firkin (View, Attr(..), element, tap, text)

type Model = WrappedEq Boolean

singleton
  :: forall k v dv
   . Ord k
  => ChangeStructure v dv
  => k
  -> D1 (v -> WrappedMap k v)
singleton k = lam \(D1 v dv) -> D1 (wrap (Map.singleton k v)) (toChange (wrap (Map.singleton k (Update (fromChange dv)))))

ifThenElse :: forall a da. ChangeStructure a da => D1 (WrappedEq Boolean -> a -> a -> a)
ifThenElse =
  lam \(D1 b db) ->
    lam \(D1 t dt) ->
      lam \(D1 f df) ->
        let go true Nothing = dt
            go false Nothing = df
            go true (Just true) = dt
            go false (Just false) = df
            go true (Just false) = toChange (diff t f <> fromChange df)
            go false (Just true) = toChange (diff f t <> fromChange dt)
        in D1 (if unwrap b then t else f)
              (go (unwrap b) (unwrap (fromChange db)))

view :: D1 (Model -> View Model)
view = lam \b ->
    element "button"
      `app` constant (wrap (Map.singleton "click" (Handler \_ -> toChange (Last (Just true)))))
      `app` (singleton "label" `app` (text `app` label b))
  where
    label b = ifThenElse
                `app` b
                `app` constant (wrap "You just triggered a model change")
                `app` constant (wrap "Click me, I'm incremental!")

main :: forall eff. Eff (console :: CONSOLE, dom :: DOM, ref :: REF | eff) Unit
main = do
  document <- map htmlDocumentToNonElementParentNode (window >>= document)
  container <- getElementById (wrap "container") document
  case container of
    Just el -> tap el view (wrap false)
    Nothing -> log "No 'container' node!"
