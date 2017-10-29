{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecursiveDo #-}

module Brick.BananaMain
  ( brickNetwork
  , Next(..)
    -- * same interface but without forcing the user to use mfix
  , brickNetworkNoFix
  )
where



import qualified Reactive.Banana as Banana
import qualified Reactive.Banana.Frameworks as Banana

import           Brick.Types                      ( Widget
                                                  , locationRowL
                                                  , locationColumnL
                                                  , CursorLocation(..)
                                                  , Extent
                                                  )
import           Brick.Types.Internal             ( RenderState(..)
                                                  )
import           Brick.Widgets.Internal           ( renderFinal
                                                  )
import           Brick.AttrMap

import qualified Data.Map as M
import qualified Data.Set as S

import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad
import           Control.Monad.Fix                (mfix)
import           Control.Concurrent
import           Control.Exception                (finally)
import           Lens.Micro                       ((^.), (<&>))
import           Data.IORef

import           Graphics.Vty
                                                  ( Vty
                                                  , Picture(..)
                                                  , Cursor(..)
                                                  , Event(..)
                                                  , update
                                                  , outputIface
                                                  , inputIface
                                                  , displayBounds
                                                  , shutdown
                                                  , mkVty
                                                  , defaultConfig
                                                  )
import           Graphics.Vty.Input               ( _eventChannel
                                                  )
import           Control.Concurrent.STM.TChan
import           Control.Monad.STM

-- import qualified System.IO.Unsafe as Unsafe



data Next
  = Redraw
  | Halt

instance Monoid Next where
  mempty = Redraw
  mappend Redraw Redraw = Redraw
  mappend _      _      = Halt


-- | Main interface of the reactive-banana interface to brick. This interface
-- is designed in such a way that you will most likely have to make use of
-- the MonadFix instance of 'MomentIO' one way or another
-- (@mdo@\/@do rec@\/@mfix@), because one of the return values of @brickNetwork@
-- is the input event that most likely determines the content of the widgets
-- to draw.
--
-- All \'Event\'s other than the one in \'Maybe Event\' are /reactive-banana/
-- @Event@s.
-- The remaining one is brick's "input" event type.
-- 
-- For the startup event, the handler must be provided as well because it
-- is used internally, too. The handler must be called once by the user.
--
-- For the suspending-functionality, the following caveats apply:
-- 
--     * Starting a second IO-action before all previously started ones
--       have "returned" leads is undefined behaviour (i.e. the current
--       implementation might not even 'error' out but fail in some
--       random other fashion). (If I tried to phrase this in terms of Events,
--       it would enlighten neither of us.)
--
--     * Sending a "halt" signal to this brick interface while this
--       is in suspended state will properly shut down the brick network
--       but it will not stop the IO-action itself. Should this scenario
--       become relevant, the user is free to killThread the action
--       manually (but bare in mind that the action will run in a forked
--       thread, so the action would have to pass out its ThreadId via
--       MVar or such as the first thing it does).
--
--     * It is possible but generally not recommended to use the suspension
--       callback function multiple times. If suspensions don't overlap it
--       will probably work, but it is not tested. Also, in theory we could
--       not bind the @a@ at top-level, i.e. make the callback have type
--       @forall a . Banana.Event (IO a) -> Banana.MomentIO (Banana.Event a)@.
--       If you ever find that useful, please open an issue.
--
-- Apart from the documentation here, please also consider the documentation
-- of the non-reactive-banana brick interface.
brickNetwork
  :: forall n a
   . Ord n
  => (Banana.AddHandler (), Banana.Handler ())
     -- ^ startup event and handler. handler must be fired once at program
     -- start to get things in motion (after actuating the network).
  -> Banana.Event Next
     -- ^ signal brick to either redraw or halt.
  -> Banana.Behavior [Widget n]
     -- ^ behaviour that contains the widget(s) to draw when redrawing
  -> Banana.Behavior
       ([CursorLocation n] -> Maybe (CursorLocation n))
     -- ^ behaviour that contains the cursor selection function
  -> Banana.Behavior AttrMap
     -- ^ behaviour that contains the attribute-map
  -> Banana.MomentIO
       ( Banana.Event (Maybe Event)
       , Banana.Event ()
       , (  Banana.Event (IO a)
         -> Banana.MomentIO (Banana.Event a)
         )
       )
     -- ^ returns three results:
     -- 1) A (reactive-banana) event containing any (brick) input
     --    events
     -- 2) An event that fires once when shutdown of the brick interface is
     --    completed
     -- 3) a function that can be used to implement suspension of the brick
     --    ui to run some external commands etc. see the demo program for
     --    usage.
brickNetwork (startupAH, startupH) triggerE widgetB cursorB attrB = do
  let initialRS = RS M.empty [] S.empty mempty []

  (eventEvent   , eventH   ) <- Banana.newEvent
  (shutdownEvent, shutdownH) <- Banana.newEvent
  startupEvent               <- Banana.fromAddHandler startupAH
  (redrawE , redrawH )       <- Banana.newEvent
  (suspendE, suspendH)       <- Banana.newEvent

  Banana.reactimate $ (void . forkIO) <$> suspendE

  let suspendSetup
        :: forall a . Banana.Event (IO a) -> Banana.MomentIO (Banana.Event a)
      suspendSetup ioE = do
        (resultE, resultH) <- Banana.newEvent
        Banana.reactimate $ ioE <&> \io ->
          void $ suspendH $ ((io >>= resultH) `finally` startupH ())
        return resultE


  initState <- mfix $ \initState -> do
    let
      e1 = startupEvent <&> \() -> liftIO $ do
        vty <- do
          x <- mkVty defaultConfig
          return x
        haltIORef <- newIORef False
        let loop = do
              ev <- atomically $ readTChan $ _eventChannel $ inputIface vty
              shouldHalt <- readIORef haltIORef
              unless shouldHalt $ do
                case ev of
                  (EvResize _ _) ->
                    eventH
                      .   Just
                      .   (\(w, h) -> EvResize w h)
                      =<< (displayBounds $ outputIface vty)
                  _ -> eventH $ Just ev
                loop
        void $ forkIO $ loop
        void $ forkIO $ eventH $ Nothing
        let stopper = do
              writeIORef haltIORef True
              void $ forkIO $ atomically $ writeTChan
                (_eventChannel $ inputIface vty)
                undefined
              shutdown vty
        return $ pure $ Just (vty, stopper)
    let e2 = flip Banana.apply suspendE $ initState <&> \mState _ ->
          liftIO $ do
            mState `forM_` snd
            return $ pure Nothing
        e3 = Banana.filterJust $ triggerE <&> \case
          Redraw -> Nothing
          Halt   -> Just $ pure $ pure $ Nothing
    eB <- Banana.execute $ Banana.unionWith
      (error "brick internal error: simultaneous startup/suspend")
      e1
      (Banana.unionWith const e2 e3)
    Banana.switchB (pure Nothing) eB

  -- this approach avoids the "execute", but breaks the suspend-resume stuff
  -- probably due to unsafePerformIO not being executed a second time.
  -- initState                           <- do
  --   let
  --     e1 = startupEvent <&> \() _ -> Unsafe.unsafePerformIO $ do
  --       vty <- liftIO $ do
  --         x <- mkVty def
  --         return x
  --       haltIORef <- newIORef False
  --       let loop = do
  --             ev <- atomically $ readTChan $ _eventChannel $ inputIface vty
  --             shouldHalt <- readIORef haltIORef
  --             unless shouldHalt $ do
  --               case ev of
  --                 (EvResize _ _) ->
  --                   eventH
  --                     .   Just
  --                     .   (\(w, h) -> EvResize w h)
  --                     =<< (displayBounds $ outputIface vty)
  --                 _ -> eventH $ Just ev
  --               loop
  --       void $ forkIO $ loop
  --       void $ forkIO $ eventH $ Nothing
  --       let stopper = do
  --             writeIORef haltIORef True
  --             void $ forkIO $ atomically $ writeTChan
  --               (_eventChannel $ inputIface vty)
  --               undefined
  --             shutdown vty
  --       return $ Just (vty, stopper)
  --   let e2 = suspendE <&> \_ mState -> Unsafe.unsafePerformIO $ do
  --         mState `forM_` snd
  --         return Nothing
  --   let e3 = Banana.filterJust $ triggerE <&> \case
  --         Redraw -> Nothing
  --         Halt   -> Just $ const $ Nothing
  --   Banana.accumB Nothing $ Banana.unionWith
  --     (error "brick internal error: simultaneous startup/suspend")
  --     e1
  --     (Banana.unionWith const e2 e3)

  Banana.reactimate
    $   flip Banana.apply triggerE
    $   initState
    <&> \mState -> \case
          Redraw -> redrawH ()
          Halt   -> do
            mState `forM_` snd
            shutdownH ()

  rsRef <- liftIO $ newIORef initialRS

  let redrawF
        :: Maybe (Vty, IO ())
        -> [Widget n]
        -> ([CursorLocation n] -> Maybe (CursorLocation n))
        -> AttrMap
        -> IO ()
      redrawF mState widgetStack chooseCursor attrs = do
        case mState of
          Nothing       -> pure ()
          Just (vty, _) -> do
            renderState           <- readIORef rsRef
            (renderState', _exts) <- render vty
                                            widgetStack
                                            chooseCursor
                                            attrs
                                            renderState
            writeIORef rsRef renderState'

  Banana.reactimate
    $         redrawF
    <$>       initState
    <*>       widgetB
    <*>       cursorB
    <*>       attrB
    Banana.<@ redrawE

  pure (eventEvent, shutdownEvent, suspendSetup)



-- | Firstly, it is strongly recommended to use 'brickNetwork' instead, as
-- it exposes exactly the same functionality.
--
-- I'll just put a well-formatted version of the type signature
-- here, which makes this prone to become
-- out of date (yay..) but allows me to properly tag everything (yay!)
--
-- > brickNetworkNoFix
-- >   :: forall n
-- >    . Ord n
-- >   => (Banana.AddHandler (), Banana.Handler ())        -- i1, i2: startup event/handler
-- >   -> (  Banana.Event (Maybe Event)                    -- s1: brick event source
-- >      -> Banana.Event ()                               -- s2: post shutdown event
-- >      -> ( forall a . Banana.Event (IO a)
-- >           -> Banana.MomentIO (Banana.Event a)
-- >         )                                             -- s3: suspender with "callback"
-- >      -> Banana.MomentIO
-- >           ( Banana.Event Next                         -- o1: halt or redraw signal
-- >           , Banana.Behavior [Widget n]                -- o2: widget layers to draw
-- >           , Banana.Behavior                           -- o3: cursor selection function
-- >               ([CursorLocation n] -> Maybe (CursorLocation n))
-- >           , Banana.Behavior AttrMap                   -- o4: brick global attribute map
-- >           )
-- >      )
-- >   -> Banana.MomentIO ()
--
-- All \'Event\'s other than the one in __s1__ are /reactive-banana/ @Event@s.
-- The lonely one is brick's "input" event type.
--
-- [i1, i2]: Event and corresponding Handler connected to startup. The event
-- should be fired once by the user (and the Handler will be used internally).
--
-- [s1]: Just Event or redraw-trigger
--
-- [s2]: Fires after shutdown of wrapper
--
-- [s3]:
--     Callback to register IO-actions to run while brick is suspended.
--     Results are returned in the result.
--
--     Note that:
--
--     a) Starting a second IO-action before all previously started ones
--        have "returned" leads is undefined behaviour (i.e. the current
--        implementation might not even 'error' out but fail in some
--        random other fashion). (If I tried to phrase this in terms of Events,
--        it would enlighten neither of us.)
--
--     b) Sending a "halt" signal to this brick interface while this
--        is in suspended state will properly shut down the brick network
--        but it will not stop the IO-action itself. Should this scenario
--        become relevant, the user is free to killThread the action
--        manually (but bare in mind that the action will run in a forked
--        thread, so the action would have to pass out its ThreadId via
--        MVar or such as the first thing it does).
--
-- [o1]: initiates shutdown of brick
--
-- [o2, o3, o4]:
--     as the short description says; see the non-brick interface for
--     details.
brickNetworkNoFix
  :: forall a n
   . Ord n
  => (Banana.AddHandler (), Banana.Handler ())
  -> (  Banana.Event (Maybe Event)
     -> Banana.Event ()
     -> (Banana.Event (IO a) -> Banana.MomentIO (Banana.Event a))
     -> Banana.MomentIO
          ( Banana.Event Next
          , Banana.Behavior [Widget n]
          , Banana.Behavior
              ([CursorLocation n] -> Maybe (CursorLocation n))
          , Banana.Behavior AttrMap
          )
     ) -- ^ overly-long-line
  -> Banana.MomentIO ()
brickNetworkNoFix (startupAH, startupH) interfaceF = mdo
  (triggerE, widgetB, cursorB, attrB) <- interfaceF eventEvent
                                                    shutdownEvent
                                                    suspendSetup
  (eventEvent, shutdownEvent, suspendSetup) <- brickNetwork
    (startupAH, startupH)
    triggerE
    widgetB
    cursorB
    attrB
  pure ()


render
  :: Vty
  -> [Widget n]
  -> ([CursorLocation n] -> Maybe (CursorLocation n))
  -> AttrMap
  -> RenderState n
  -> IO (RenderState n, [Extent n])
render vty widgetStack chooseCursor attrMapCur rs = do
  sz <- displayBounds $ outputIface vty
  let (newRS, pic, theCursor, exts) =
        renderFinal attrMapCur widgetStack sz chooseCursor rs
      picWithCursor = case theCursor of
        Nothing  -> pic { picCursor = NoCursor }
        Just loc -> pic
          { picCursor = AbsoluteCursor (loc ^. locationColumnL)
                                       (loc ^. locationRowL)
          }

  update vty picWithCursor

  return (newRS, exts)

redraw :: Next
redraw = Redraw

halt :: Next
halt = Halt
