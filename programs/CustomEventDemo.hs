{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Monad (void, forever)
import Control.Concurrent (threadDelay, forkIO)
import Data.Monoid
import qualified Graphics.Vty as V
import Control.Concurrent.MVar

import Brick.Types
import Brick.Widgets.Core

import qualified Reactive.Banana as Banana
import qualified Reactive.Banana.Frameworks as Banana
import           Brick.BananaMain
import           Graphics.Vty(defAttr)
import           Brick.AttrMap(attrMap)

import Lens.Micro ((<&>))



drawUI :: Maybe V.Event -> Int -> [Widget ()]
drawUI lastEvent count = [a]
 where
  a =
    (str $ "Last Vty event: " <> (show lastEvent))
      <=> (str $ "Counter value is: " <> (show count))


main :: IO ()
main = do

  finMVar                       <- newEmptyMVar
  startup@(_        , startupH) <- Banana.newAddHandler

  (        counterAH, counterT) <- Banana.newAddHandler

  network <- Banana.compile $ mdo

    (eventE, finE, _) <- brickNetwork startup  ( Banana.unionWith (<>) nextE1 nextE2)
      (drawUI <$> lastEvent <*> counter)
      (pure $ const Nothing)
      (pure $ attrMap defAttr [])

    Banana.reactimate $ finE <&> \() -> putMVar finMVar ()

    counterE  <- Banana.fromAddHandler counterAH

    counter   <- Banana.accumB 0 $ counterE <&> const (+1)
    lastEvent <- Banana.stepper Nothing eventE

    let nextE1 = eventE <&> \case
          Just (V.EvKey V.KEsc _) -> Halt
          _                       -> Redraw
    let nextE2 = counterE <&> const Redraw

    pure ()


  Banana.actuate network
  startupH ()
  void $ forkIO $ forever $ do
    counterT ()
    threadDelay 1000000
  takeMVar finMVar
  Banana.pause network
