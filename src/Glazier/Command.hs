{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.Command
    ( Commands
    , HasCommands(..)
    , Cmd(..)
    , stamp
    , stamp'
    , post
    , post'
    , inquire
    , conclude
    , concurringly
    , concurringly_
    , concur
    , AsConcur
    , ConcurCmd
    , Concur(..)
    , MkMVar -- Hiding constructor
    , unMkMVar
    ) where


import Control.Applicative
import Control.Concurrent
import Control.Lens
import Control.Monad.Cont
import Control.Monad.Delegate
import Control.Monad.State.Strict
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Semigroup

----------------------------------------------
-- Command utilties
----------------------------------------------

type Commands cmd = DL.DList cmd

class HasCommands s cmd | s -> cmd where
    _commands :: Lens' s (Commands cmd)

instance HasCommands (DL.DList cmd) cmd where
    _commands = id

-- | Adds a handler to polymorphic commands that produce a value
data Cmd f cmd where
    Cmd :: Show (f a) => f a -> (a -> cmd) -> Cmd f cmd
    Cmd_ :: Show (f ()) => f () -> Cmd f cmd

instance Show (Cmd f cmd) where
    showsPrec p (Cmd f _) = showParen (p >= 11) $
        showString "Cmd " . shows f
    showsPrec p (Cmd_ f) = showParen (p >= 11) $
        showString "Cmd_ " . shows f

-- | convert a request type to a command type.
-- This is used for commands that doesn't have a continuation.
-- Ie. commands that doesn't "returns" a value from running an effect.
-- Use 'stamp'' for commands that require a continuation ("returns" a value).
stamp :: (AsFacet c cmd) => c -> cmd
stamp = review facet

-- | A variation of 'stamp' for commands with a type variable @cmd@,
-- which is usually commands that are containers of command,
-- or commands that require a continuation
-- Eg. commands that "returns" a value from running an effect.
stamp' :: (AsFacet (c cmd) cmd) => c cmd -> cmd
stamp' = review facet

-- | Add a command to the list of commands for this state tick.
-- I basically want a Writer monad, but I'm using a State monad
-- because but I also want to use it inside a ContT which only has an instance of MonadState.
post :: (MonadState (DL.DList cmd) m, AsFacet c cmd) => c -> m ()
post c = _commands %= (`DL.snoc` (stamp c))

-- | Add a higher kinded command to the list of commands for this state tick.
-- I basically want a Writer monad, but I'm using a State monad
-- because but I also want to use it inside a ContT which only has an instance of MonadState.
post' :: (MonadState (DL.DList cmd) m, AsFacet (c cmd) cmd) => c cmd -> m ()
post' c = _commands %= (`DL.snoc` (stamp' c))

-- | Adds the ContT monad's commands into the 'MonadState' of commands.
-- 'inquire' is used to start usages of 'conclude'.
inquire :: MonadState (DL.DList cmd) m => ContT () (State (DL.DList cmd)) () -> m ()
inquire = (\s -> _commands %= (<> execState s mempty)) . evalContT

-- | This converts a command that requires a handler into a ContT monad so that the do notation
-- can be used to compose the handler for that command.
-- 'conclude' is used inside an 'inquire' block.
conclude :: (AsFacet (c cmd) cmd, AsFacet [cmd] cmd) => ((a -> cmd) -> c cmd) -> ContT () (State (DL.DList cmd)) a
conclude m = ContT $ \k -> post' $ m (stamp' @[] . DL.toList . (`execState` mempty) . k)

----------------------------------------------
-- Batch independant commands
----------------------------------------------

type AsConcur cmd = (AsFacet (ConcurCmd cmd) cmd)
type ConcurCmd cmd = Cmd (Concur cmd) cmd

-- | This monad is intended to be used with @ApplicativeDo@ to allow do notation
-- for composing commands that can be run concurrently.
-- The 'Applicative' instance can merge multiple commands into the internal state of @DList c@.
-- The 'Monad' instance creates a 'ConcurCmd' command before continuing the bind.
newtype Concur c a = Concur
    -- The base IO doesn't block (only does newEmptyMVar), but the returns an IO that blocks.
    { runConcur :: StateT (DL.DList c) MkMVar (IO a)
    }

instance Show (Concur c a) where
    showsPrec _ _ = showString "Concur"

-- | NB. Don't export MkMVar constructor to guarantee
-- that that it only contains non-blocking 'newEmptyMVar' IO.
newtype MkMVar a = MkMVar (IO a)
    deriving (Functor, Applicative, Monad)

unMkMVar :: MkMVar a -> IO a
unMkMVar (MkMVar m) = m

-- | Allows usages  of 'concur' inside a 'concurringly' block.
-- This resuls in a command that requires a handler, which may be used by 'conclude'
concurringly :: Concur cmd a -> (a -> cmd) -> ConcurCmd cmd
concurringly = Cmd

-- | Allows usages  of 'concur' inside a 'concurring' block.
-- This results in a command that doesn't require a handler and may be 'post''ed.
concurringly_ :: Concur cmd () -> ConcurCmd cmd
concurringly_ = Cmd_

instance (AsConcur cmd) => MonadState (DL.DList cmd) (Concur cmd) where
    state m = Concur $ pure <$> state m

instance Functor (Concur cmd) where
    fmap f (Concur m) = Concur $ fmap f <$> m

-- | Applicative instand allows building up list of commands without blocking
instance Applicative (Concur cmd) where
    pure = Concur . pure . pure
    (Concur f) <*> (Concur a) = Concur $ liftA2 (<*>) f a

-- Monad instance can't build commands without blocking.
instance (AsConcur cmd) => Monad (Concur cmd) where
    (Concur m) >>= k = Concur $ do
        m' <- m -- get the blocking io action while updating the state
        v <- lift $ MkMVar newEmptyMVar
        post' $ concurringly (Concur $ pure m')
            (\a -> stamp' $ concurringly (k a)
                (\b -> stamp' $ concurringly_ (Concur $ pure $ putMVar v b)))
        pure $ takeMVar v

-- | Concurrent version of 'conclude'. Converts a command that requires a handler to a Concur monad
-- so that the do notation can be used to compose the handler for that command.
-- The Concur monad allows schedule the command in concurrently with other 'concur'red commands.
-- 'concur' is used inside an 'concurringly' or 'concurringly_' block.
concur :: (AsConcur cmd, AsFacet (c cmd) cmd) => ((a -> cmd) -> c cmd) -> Concur cmd a
concur k = Concur $ do
    v <- lift $ MkMVar newEmptyMVar
    post' $ k (\a -> stamp' $ concurringly_ (Concur $ pure $ putMVar v a))
    pure $ takeMVar v
