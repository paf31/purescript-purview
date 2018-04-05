module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import DOM.Event.EventTarget (eventListener)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import Data.Incremental (class Patch, constant)
import Data.Incremental.Array (IArray)
import Data.Incremental.Array as IArray
import Data.Incremental.Eq (Atomic)
import Data.Incremental.Eq as Atomic
import Data.Incremental.Map as IMap
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (wrap)
import Purview (Component, element, element_, run, text)

type Counter = Atomic Int

counter :: forall eff. Component Counter eff
counter change model =
    element "button" (constant (wrap mempty))
      -- ^ a <button> with no attributes
      (IMap.singleton "click" (Atomic.lift2 onClick change model))
      -- ^ onclick handler
      (IArray.singleton (text (Atomic.map (("Current value = " <> _) <<< show) model)))
  where
    onClick f current = eventListener \_ ->
      f (Atomic.replace (current + 1))

listOf
  :: forall model change eff
   . Patch model change
  => model
  -> Component model eff
  -> Component (IArray model) eff
listOf dflt component change xs =
  let addCounter = Atomic.map (\change_ -> eventListener \_ ->
        change_ (IArray.insertAt 0 dflt)) change
   in element_ "div" $ IArray.static
        [ element "button" (constant (wrap mempty))
            (IMap.singleton "click" addCounter)
            (IArray.singleton (text (constant (wrap "Add"))))
        , element_ "ol" $
            IArray.mapWithIndex (\i x ->
              let changeAt i_ change_ c = change_ (IArray.modifyAt i_ c)
                  delete = Atomic.lift2 (\i_ change_ -> eventListener \_ ->
                    change_ (IArray.deleteAt i_)) i change
               in element_ "li" $ IArray.static
                    [ component (Atomic.lift2 changeAt i change) x
                    , element "button" (constant (wrap mempty))
                        (IMap.singleton "click" delete)
                        (IArray.singleton (text (constant (wrap "Remove"))))
                    ]
                  ) xs
        ]

main :: Eff (dom :: DOM, console :: CONSOLE, ref :: REF) Unit
main = do
  document <- map htmlDocumentToNonElementParentNode (window >>= document)
  container <- getElementById (wrap "container") document
  case container of
    Just el -> run el (listOf (wrap 0) counter) (wrap [])
    Nothing -> log "No 'container' node!"
