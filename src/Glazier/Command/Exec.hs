{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.Command.Exec where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Extras
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Foldable
import Data.Proxy
import Glazier.Command
import qualified UnliftIO.Async as U

-- | This defines the list of commands for a particular "app"
type family AppCmds (app :: k1) c :: [k2]

-- | This is used to define the sum of all commands
-- where the commands might recursively refer to this sum command.
data family AppCmd (app :: k1)

-- | The instance of AppCmd which recursively refers to itself
newtype instance AppCmd app = AppCmd (Which (AppCmds app (AppCmd app)))

unAppCmd :: forall app. AppCmd app -> Which (AppCmds app (AppCmd app))
unAppCmd (AppCmd w) = w

-- | Define AsFacet instances for all types in the variant
-- UndecidableInstances!
instance (AsFacet a (Which (AppCmds app (AppCmd app)))) => AsFacet a (AppCmd app) where
    facet = iso unAppCmd AppCmd . facet

-- | Create an executor for a variant in the command type.
-- returns a tuple with a 'Proxy' to keep track of the the types handled by the executor.
maybeExec :: (Applicative m, Cmd a c) => (a -> m b) -> (Proxy '[a], c -> MaybeT m b)
maybeExec k = (Proxy, \c -> MaybeT . sequenceA $ (k <$> preview facet c))

-- | Combines executors, keeping track of the combined list of types handled.
-- redundant-constraints: used to constrain a''
orExec :: Alternative m => (Proxy a, c -> m b) -> (Proxy a', c -> m b) -> (Proxy (Append a a'), c -> m b)
orExec (_, m) (_, n) = (Proxy, \c -> m c <|> n c)
infixl 3 `orExec` -- like <|>

-- | Tie an executor with itself to get the final interpreter
fixExec :: Functor m => ((c -> m ()) -> c -> MaybeT m ()) -> c -> m ()
fixExec fexec = (`evalMaybeT` ()) . fexec (fixExec fexec)

-- -- | A variation of 'fixExec' for executors that return cmds that should be evaluated next
-- fixExec' :: Monad m => ((c -> m ()) -> c -> MaybeT m [c]) -> c -> m ()
-- fixExec' fexec c = do
--     let execCmd = (`evalMaybeT` []) . fexec (fixExec' fexec) -- c -> MaybeT m [c]
--         execCmds cs = do -- [c] -> MaybeT m ()
--             case cs of
--                 [] -> pure () -- no more extra commands to process, break out of loop
--                 cs' -> do
--                     -- process the extra commands, and get extra commands to process
--                     cs'' <- (DL.concat . (fmap DL.fromList)) <$> traverse execCmd cs'
--                     -- recusrively process the extra commands
--                     execCmds $ DL.toList cs''
--     cs <- execCmd c
--     execCmds cs

-- | Use this function to verify at compile time that the given executor's Proxy will fullfill
-- all the variant types in a command type.
-- The types in the command does not have to be in the same order as the types in the Proxy.
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

-- -- 'verifyExec' and 'fixExec'' an executor.
-- fixVerifyExec' ::
--     ( AppendUnique '[] ys ~ ys
--     , AppendUnique xs ys ~ xs
--     , AppendUnique ys xs ~ ys
--     , Monad m
--     ) => (c -> Which xs) -> ((c -> m ()) -> (Proxy ys, c -> MaybeT m [c])) -> c -> m ()
-- fixVerifyExec' unCmd maybeExecuteCmd = fixExec' (verifyExec unCmd . maybeExecuteCmd)


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
    readAndExecute x = do
        as <- liftIO x
        -- for each value obtained, fire them back to the executor as
        executeCmds as
    execConcur_ = do
        -- get the list of commands to run
        (r, cs) <- liftIO . runProgramT' $ runConcur c
        -- run the batched commands in separate threads
        -- these should produce and write values to the bus channel
        executeCmds (DL.toList cs)
        -- now it is ok to return the io to read from the TQueue
        pure r
    executeCmds cs = case cs of
        [c'] -> executor c'
        cs' -> do
            cs'' <- traverse (U.async . executor) cs'
            -- now wait for all the threads to finish
            traverse_ U.wait cs'' -- shouldn't have exceptions!


execIO :: MonadIO m => (c -> m ()) -> IO c -> m ()
execIO executor m = liftIO m >>= executor
