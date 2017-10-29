{-# LANGUAGE RecursiveDo #-}

module Main where

import Data.Monoid
import qualified Graphics.Vty as V
import Control.Concurrent.MVar
import Lens.Micro                       ( (<&>) )

import Brick.Types                      ( Widget )
import Brick.Widgets.Core               ( vBox
                                        , str
                                        )
import qualified Reactive.Banana as Banana
import qualified Reactive.Banana.Frameworks as Banana
import           Brick.BananaMain
import           Graphics.Vty(defAttr)
import           Brick.AttrMap(attrMap)



drawUI :: String -> [Widget ()]
drawUI st = [ui]
 where
  ui = vBox
    [ str $ "External input: \"" <> st <> "\""
    , str "(Press Esc to quit or Space to ask for input)"
    ]


main :: IO ()
main = do

  finMVar               <- newEmptyMVar
  startup@(_, startupH) <- Banana.newAddHandler

  network               <- Banana.compile $ mdo

    (eventE, finE, suspendCB) <- brickNetwork startup
                                              nextE
                                              (drawUI <$> resultB)
                                              (pure $ const Nothing)
                                              (pure $ attrMap defAttr [])

    Banana.reactimate $ finE <&> \() -> putMVar finMVar ()

    resultE <- suspendCB $ Banana.filterJust $ eventE <&> \case
      Just (V.EvKey (V.KChar ' ') []) -> Just $ do
        putStrLn "Suspended. Please enter something and press enter to resume:"
        getLine
      _ -> Nothing

    resultB <- Banana.stepper "" resultE

    let nextE = eventE <&> \case
          Just (V.EvKey V.KEsc _) -> Halt
          _                       -> Redraw

    pure ()

  Banana.actuate network
  startupH ()
  takeMVar finMVar
  Banana.pause network
