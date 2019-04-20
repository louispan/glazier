{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Glazier.Command.Exec where

import Data.Kind
import Control.Applicative
import Control.Lens
import Control.Monad.IO.Unlift
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Maybe.Extras
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Foldable
import Data.Proxy
import Glazier.Command
import qualified UnliftIO.Async as U

-- | type function to get the list of effects in a @cmd@, parameterized over @c@
type family CmdTypes cmd c :: [Type]

-- | A command type that removes the @IO c@ from the @CmdTypes@ of the input @c@
newtype NoIOCmd c = NoIOCmd { unNoIOCmd :: Which (CmdTypes (NoIOCmd c) (NoIOCmd c)) }

-- | Removes the @IO c@ from the @CmdTypes@ of the input @c@
type instance CmdTypes (NoIOCmd cmd) c = Remove (IO c) (CmdTypes cmd c)

-- UndecidableInstances!
instance (AsFacet a (Which (CmdTypes (NoIOCmd c) (NoIOCmd c)))) => AsFacet a (NoIOCmd c) where
    facet = iso unNoIOCmd NoIOCmd . facet

-- | Create an executor for a variant in the command type.
-- returns a tuple with a 'Proxy' to keep track of the the types handled by the executor.
maybeExec :: (Applicative m, AsFacet a c) => (a -> m b) -> (Proxy '[a], c -> MaybeT m b)
maybeExec k = (Proxy, \c -> MaybeT . sequenceA $ (k <$> preview facet c))

-- | Combines executors, keeping track of the combined list of types handled.
-- redundant-constraints: used to constrain a''
orExec :: Alternative m => (Proxy a, c -> m b) -> (Proxy a', c -> m b) -> (Proxy (Append a a'), c -> m b)
orExec (_, m) (_, n) = (Proxy, \c -> m c <|> n c)
infixl 3 `orExec` -- like <|>

-- | Tie an executor with itself to get the final interpreter
fixExec :: Functor m => ((c -> m ()) -> c -> MaybeT m ()) -> c -> m ()
fixExec fexec = (`evalMaybeT` ()) . fexec (fixExec fexec)

-- | A variation of 'fixExec' for executors that return cmds that should be evaluated last
fixExec' :: Monad m => ((c -> m ()) -> c -> MaybeT m [c]) -> c -> m ()
fixExec' fexec c = do
    let execCmd = (`evalMaybeT` []) . fexec (fixExec' fexec) -- c -> MaybeT m [c]
        execCmds cs = do -- [c] -> MaybeT m ()
            case cs of
                [] -> pure () -- no more extra commands to process, break out of loop
                cs' -> do
                    -- process the extra commands, and get extra commands to process
                    cs'' <- (DL.concat . (fmap DL.fromList)) <$> traverse execCmd cs'
                    -- recusrively process the extra commands
                    execCmds $ DL.toList cs''
    cs <- execCmd c
    execCmds cs

-- | Use this function to verify at compile time that the given executor's Proxy will fullfill
-- all the variant types in a command type.
-- THe types in the command does not have to be in the same order as the types in the Proxy.
-- There can't be any extra types in ys or xs.
-- redundant-constraints: used to constrain xs and ys
verifyExec ::
    ( AppendUnique '[] ys ~ ys -- no duplicate types in ys
    , AppendUnique xs ys ~ xs -- no extra types in ys
    , AppendUnique ys xs ~ ys -- no no extra types in xs
    )
    => (c -> Which xs) -> (Proxy ys, a) -> a
verifyExec _commandToTypes (_typesOfExecutor, a) = a

-- 'verifyExec' and 'fixExec' an executor.
fixVerifyExec ::
    ( AppendUnique '[] ys ~ ys
    , AppendUnique xs ys ~ xs
    , AppendUnique ys xs ~ ys
    , Functor m
    ) => (c -> Which xs) -> ((c -> m ()) -> (Proxy ys, c -> MaybeT m ())) -> c -> m ()
fixVerifyExec unCmd maybeExecuteCmd = fixExec (verifyExec unCmd . maybeExecuteCmd)

-- 'verifyExec' and 'fixExec'' an executor.
fixVerifyExec' ::
    ( AppendUnique '[] ys ~ ys
    , AppendUnique xs ys ~ xs
    , AppendUnique ys xs ~ ys
    , Monad m
    ) => (c -> Which xs) -> ((c -> m ()) -> (Proxy ys, c -> MaybeT m [c])) -> c -> m ()
fixVerifyExec' unCmd maybeExecuteCmd = fixExec' (verifyExec unCmd . maybeExecuteCmd)


execConcur ::
    MonadUnliftIO m
    => (c -> m ())
    -> Concur c c
    -> m ()
execConcur executor c = do
        r <- execConcur_
        case r of
            -- Now run the io to read from the TQueue
            ConcurRead x -> readAndExecute x
            ConcurPure x -> executor x
  where
    -- we run mx multiple times until BlockedIndefinitelyOnMVar
    -- to make sure we have drained all the data from the Chan.
    -- forkIO discards BlockedIndefinitelyOnMVar.
    -- DANGER! If the Left IO contains something that will never fail
    -- then this thread will never be cleaned up.
    readAndExecute x = do
        as <- liftIO (unNonBlocking x)
        -- for each value obtained, fire them back to the executor
        as' <- traverse (U.async . executor) as
        -- now wait for all the threads to finish
        traverse_ (void . U.waitCatch) as'
    execConcur_ = do
        -- get the list of commands to run
        (r, cs) <- liftIO $ unNonBlocking $ (`runStateT` mempty) $ runProgramT $ runConcur c
        -- run the batched commands in separate threads
        -- these should produce and write values to the bus channel
        as <- traverse (U.async . executor) (DL.toList cs)
        -- now wait for all the threads to finish writing to TQueue
        traverse_ (void . U.waitCatch) as
        -- return the io to read from the TQueue
        pure r

execIO :: MonadIO m => (c -> m ()) -> IO c -> m ()
execIO executor m = liftIO m >>= executor
