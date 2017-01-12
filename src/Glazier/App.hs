module Glazier.App where

import Control.Lens
import Control.Monad.Morph
import Control.Monad.Trans.Reader.Extras
import Control.Monad.State.Strict
import Glazier.Core

-- | Runs one tick of the notify processing, given handler functions, and stores
-- the updated state of the model.
-- * tickState is tick procesing function that returns commands,
--   rendering frame, and updated model state.
-- * onFrame is given the state to render, but is not a State Monad so it cannot modify the state
-- * interpretCommands is given the commands, and is not a State Monad so interpreting the commands
--   is not dependent on the state. This forces the interpreter to be more explicit.
-- An example of tickState function using Pipes.Concurrent is:
-- @
--  import qualified Pipes.Concurrent as PC.
--  import qualified Control.Monad.Trans.State.Strict.Extras as SE
--
--  run = do
--    (address, signal, seal) <- PC.spawn' PC.unbounded
--    let mkCtl = (MaybeT . fmap guard) <$> PC.send address
--        (xs, render) = G.startWidget (appWidget mkCtl) inbox
--        tickState = SE.maybeState $ hoist (MaybeT . liftIO . atomically . PC.recv) xs
-- @
runNotify
    :: ( MFunctor t
       , MonadState s (u m)
       , MonadTrans u
       , MonadTrans t
       , Monad (t (u m))
       , Monad m
       )
    => t (u m) cmds -> (s -> t m ()) -> (cmds -> t m b) -> t (u m) b
runNotify tick renderFrame interpretCommands  = do
    cmds <- tick
    s <- lift get
    hoist lift $ renderFrame s
    hoist lift $ interpretCommands cmds

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
