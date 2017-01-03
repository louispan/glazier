module Glazier.App where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Glazier.Core

-- | This function combines two different monadic effects.
--
-- This can enable past-Dependence.
-- Elm has foldp : (a -> state -> state) -> state -> Signal a -> Signal state
-- This is equivalent to a creating a @StateT state (Signal m) ()@
--
-- 'runReaderM' is a more general form of @StateT state (Signal m) ()@ where
-- given a reader monad to transform "a" to "c" with effects, and an "as"
-- monad that produces "a"s with other effects, run the result of "as" through
-- the reader monad to produce "c"s with both effects.
-- @
-- runReaderM :: Monad m => Reader a (State s) c -> m a                      -> StateT s m c
-- runReaderM ::            Reader a (State s) c -> Signal STM a             -> StateT state (Signal STM) c
-- runReaderM ::            Reader a (State s) c -> Pipes.Concurrent.Input a -> StateT state Pipes.Concurrent.Input c
-- @
runReaderM :: (Monad m, Monad (t m), MonadTrans t, MFunctor t)
  => ReaderT a (t Identity) c -> m a -> t m c
runReaderM c as = do
  a <- lift as
  hoist generalize $ runReaderT c a

-- | Runs one tick of the notify processing, given handler functions, and stores
-- the updated state of the model.
-- * tickState is tick procesing function that returns commands,
--   rendering frame, and updated model state.
-- * onFrame is given the state to render.
-- * interpretCommands is given the commands.
-- An example of tickState function using Pipes.Concurrent is:
-- @
--  import Pipes.Concurrent
--
--  run = do
--    (address, signal, seal) <- PC.spawn' PC.unbounded
--    let mkCtl = (MaybeT . fmap guard) <$> PC.send address
--        (xs, render) = G.startWidget (appWidget mkCtl) inbox
--        tickState = hoist (MaybeT . liftIO . atomically . PC.recv) xs
-- @
runNotify ::
  (Monad m, MonadError s m) =>
  (s -> MaybeT m ())
  -> (cmds -> MaybeT m ())
  -> StateT s (MaybeT m) cmds
  -> StateT s m ()
runNotify onFrame interpretCommands tickState = do
    s <- get
    mcs <- lift . runMaybeT $ runStateT tickState s
    case mcs of
        Nothing -> throwError s
        Just (cmds, s') -> do
            put s'
            m <-
                lift $
                runMaybeT $
                 do
                     -- NB cmds are processed before rendering, so that we can quit without rendering again
                    interpretCommands cmds
                    onFrame s'
            -- on quit, throw the latest state in case downstream wants to use it.
            case m of
                Nothing -> throwError s'
                Just _ -> pure ()

-------------------------------------------------------------------------------
-- | A more generic form of Elm start app.
-- This uses the input Widget and input monad to result in a @StateT s m@ that:
-- * gets actions from an input monad,
-- * run the actions through the widget notify to store the notifyd model,
-- * returns commands
-- The Depict is returned as a function so that the frame can be rendered on demand
-- using the state from the StateT.
startWidget :: Monad m => Widget a s c d -> m a -> (StateT s m c, s -> d)
startWidget w signal = (rs, v)
 where
  u = w ^. notify . _Wrapped
  v =  w ^. depict . _Wrapped
  -- read from inbox of actions, and apply to Notify
  rs = runReaderM u signal
