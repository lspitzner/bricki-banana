{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where



import Brick.BananaMain
import Brick.Types

import qualified Reactive.Banana as Banana
import qualified Reactive.Banana.Frameworks as Banana

import qualified Graphics.Vty as V

import           Graphics.Vty(defAttr)
import           Brick.AttrMap(attrMap)

import Control.Concurrent.MVar

import Lens.Micro ((<&>))

import Brick.Widgets.Core



main :: IO ()
main = do

  finMVar               <- newEmptyMVar
  startup@(_, startupH) <- Banana.newAddHandler

  network               <- Banana.compile $ mdo

    (eventE, finE, _) <- brickNetwork startup nextE widgetsB cursorB attrB

    Banana.reactimate $ finE <&> \() -> putMVar finMVar ()

    curPromptStr <- Banana.accumB "" $ eventE <&> \case
      Just (V.EvKey key _mods) -> case key of
        V.KEsc    -> const ""
        V.KEnter  -> const ""
        V.KChar c -> (++[c])
        V.KBS     -> init
        _         -> id
      _ -> id


    let promptWidget :: Banana.Behavior (Widget String) =
          curPromptStr <&> \s -> str $ if null s then " " else s
    let lengthWidget :: Banana.Behavior (Widget String) =
          curPromptStr <&> str . show . length

    let nextE = eventE <&> \case
          Just (V.EvKey V.KEsc _) -> Halt
          _                       -> Redraw

    let widgetsB =
          (\wid1 wid2 -> [wid1 <=> wid2 <=> emptyWidget])
            <$> promptWidget
            <*> lengthWidget

    let cursorB = pure $ const Nothing
    let attrB   = pure $ attrMap defAttr []

    pure ()


  Banana.actuate network
  startupH ()
  takeMVar finMVar
  Banana.pause network
