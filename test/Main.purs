module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import DOM.Event.EventTarget (eventListener)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import Data.Incremental (Change, Jet, constant)
import Data.Incremental.Array as IArray
import Data.Incremental.Eq (Atomic, mapAtomic, replace)
import Data.Incremental.Map as IMap
import Data.Incremental.Monoid (Right(..), appendRight)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap, wrap)
import Firkin (View, element, tap, text)

type Model = Atomic Int

view :: (Change Model -> Eff _ Unit) -> Jet Model -> Jet (View _)
view change counter =
    element "button" (constant (wrap mempty))
      -- ^ a <button> with no attributes
      (IMap.singleton "click" (mapAtomic onClick counter))
      -- ^ onclick handler
      (IArray.singleton (text (mapAtomic (("Current value = " <> _) <<< show) counter)))
  where
    onClick current = eventListener \_ ->
      change (replace (current + 1))

main :: Eff _ Unit
main = do
  document <- map htmlDocumentToNonElementParentNode (window >>= document)
  container <- getElementById (wrap "container") document
  case container of
    Just el -> tap el view (wrap 0)
    Nothing -> log "No 'container' node!"
