module Glazier.Command.Exec where

import Control.Lens
import Control.Monad.IO.Unlift
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Foldable
import Glazier.Command
import qualified UnliftIO.Concurrent as U

maybeExec :: (Applicative m, AsFacet a c) => (a -> m b) -> c -> MaybeT m b
maybeExec k y = MaybeT . sequenceA $ k <$> preview facet y

execConcurCmd ::
    MonadUnliftIO m
    => (cmd -> m ())
    -> ConcurCmd cmd
    -> m ()
execConcurCmd exec cmd = do
    case cmd of
        (Cmd (Concur m) k) -> do
            ma <- execConcurCmd_ exec m
            -- Now run the blocking io, which produces the final command
            a <- liftIO ma
            exec (k a)
        (Cmd_ (Concur m)) -> do
            ma <- execConcurCmd_ exec m
            -- Now run the blocking io, which produces the final command
            liftIO ma
  where
    execConcurCmd_ exec' m = do
        -- get the list of commands to run
        (ma, cs) <- liftIO $ unMkMVar $ runStateT m mempty
        -- run the batched commands in separate threads
        traverse_ (void . U.forkIO . exec') (DL.toList cs)
        pure ma
