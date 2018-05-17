{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE ApplicativeDo #-}
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

module Glazier.Command where
    -- ( Commands
    -- , HasCommands(..)
    -- , HasCommands'
    -- , _commands'
    -- , Cmd(..)
    -- , Codify(..)
    -- , MonadCommand
    -- , command
    -- , command'
    -- , post
    -- , postcmd
    -- , postcmd'
    -- -- , inquire
    -- , conclude
    -- , concurringly
    -- , concurringly_
    -- , concur
    -- , AsConcur
    -- , ConcurCmd
    -- , Concur(..)
    -- , MkMVar -- Hiding constructor
    -- , unMkMVar
    -- ) where


import Control.Applicative
import Control.Concurrent
import Control.Lens
import Control.Monad.Cont
import Control.Monad.Delegate
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.AReader
import Control.Monad.Trans.AState.Lazy as Lazy
import Control.Monad.Trans.AState.Strict as Strict
import Control.Monad.Trans.Identity
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Data.Diverse.Lens
import qualified Data.DList as DL

----------------------------------------------
-- Command utilties
----------------------------------------------

-- | Adds a handler to polymorphic commands that produce a value
data Cmd f cmd where
    Cmd :: Show (f a) => f a -> (a -> cmd) -> Cmd f cmd
    Cmd_ :: Show (f ()) => f () -> Cmd f cmd

instance Show (Cmd f cmd) where
    showsPrec p (Cmd f _) = showParen (p >= 11) $
        showString "Cmd " . shows f
    showsPrec p (Cmd_ f) = showParen (p >= 11) $
        showString "Cmd_ " . shows f

-- | Converts a monad transformer stack with a 'State' of list of commands
-- to output a single command, using the current monad context.
class Codify m cmd | m -> cmd where
    codify :: (a -> m ()) -> m (a -> cmd)
    -- codify2 :: m a -> (a -> cmd) -> m cmd

-- codify1 :: (Functor m, Codify m cmd) => m () -> m cmd
-- codify1 m = (\f -> f ()) <$> codify (const m)

-- | Convert a State into a single command, but running it with mempty like Writer.
instance AsFacet [cmd] cmd => Codify (Strict.State (DL.DList cmd)) cmd where
    codify f = pure $ \a -> case DL.toList . (`Strict.execState` mempty) $ f a of
        [x] -> x
        xs -> command' xs

    -- codify2 m f = do
    --     let (a, s) = (`Strict.runState` mempty) m
    --     in pure $ command' @[] $ DL.toList $ s `DL.snoc` f a

instance AsFacet [cmd] cmd => Codify (Strict.AState (DL.DList cmd)) cmd where
    codify f = pure $ \a -> case DL.toList . (`Strict.execAState` mempty) $ f a of
        [x] -> x
        xs -> command' xs

instance AsFacet [cmd] cmd => Codify (Lazy.State (DL.DList cmd)) cmd where
    codify f = pure $ \a -> case DL.toList . (`Lazy.execState` mempty) $ f a of
        [x] -> x
        xs -> command' xs

instance AsFacet [cmd] cmd => Codify (Lazy.AState (DL.DList cmd)) cmd where
    codify f = pure $ \a -> case DL.toList . (`Lazy.execAState` mempty) $ f a of
        [x] -> x
        xs -> command' xs

instance (Codify m cmd, Monad m) => Codify (IdentityT m) cmd where
    codify f = lift . codify $ runIdentityT . f

instance (Codify m cmd, Monad m) => Codify (ContT () m) cmd where
    codify f = lift . codify $ evalContT . f

instance (Codify m cmd, Monad m) => Codify (AContT () m) cmd where
    codify f = lift . codify $ evalAContT . f

instance (Codify m cmd, Monad m) => Codify (ReaderT r m) cmd where
    codify f = do
        r <- ask
        lift . codify $ (`runReaderT` r) . f

instance (Codify m cmd, Monad m) => Codify (AReaderT r m) cmd where
    codify f = do
        r <- ask
        lift . codify $ (`runAReaderT` r) . f

type MonadCommand m cmd = (MonadState (DL.DList cmd) m, Codify m cmd)

-- | convert a request type to a command type.
-- This is used for commands that doesn't have a continuation.
-- Ie. commands that doesn't "returns" a value from running an effect.
-- Use 'command'' for commands that require a continuation ("returns" a value).
command :: (AsFacet c cmd) => c -> cmd
command = review facet

-- | A variation of 'command' for commands with a type variable @cmd@,
-- which is usually commands that are containers of command,
-- or commands that require a continuation
-- Eg. commands that "returns" a value from running an effect.
command' :: (AsFacet (c cmd) cmd) => c cmd -> cmd
command' = review facet

-- | Add a command to the list of commands for this state tick.
-- I basically want a Writer monad, but I'm using a State monad
-- because but I also want to use it inside a ContT which only has an instance of MonadState.
post :: (MonadState (DL.DList cmd) m) => cmd -> m ()
post c = id %= (`DL.snoc` c)

-- posts :: (MonadState (DL.DList cmd) m) => DL.DList cmd -> m ()
-- posts cs = id %= (<> cs)

-- | @'postcmd' = 'post' . 'command'@
postcmd :: (MonadState (DL.DList cmd) m, AsFacet c cmd) => c -> m ()
postcmd = post . command

-- | @'postcmd'' = 'post' . 'command''@
postcmd' :: (MonadState (DL.DList cmd) m, AsFacet (c cmd) cmd) => c cmd -> m ()
postcmd' = post . command'

-- | This converts a command that requires a handler into a ContT monad so that the do notation
-- can be used to compose the handler for that command.
-- Unlike 'concur', 'conclude' is only used inside an 'evalContT' block.
-- The 'MonadCont' constraint is redundant but rules out
-- using 'Concur' at the bottom of the transformer stack.
-- 'conclude' is used for operations that MUST run sequentially,
-- not concurrently.
conclude ::
    ( MonadDelegate () m
    , MonadState (DL.DList cmd) m
    , Codify m cmd
    , AsFacet (c cmd) cmd
    , MonadCont m
    )
    => ((a -> cmd) -> c cmd) -> m a
conclude = concur2

concludeM ::
    ( MonadDelegate () m
    , Codify m cmd
    , MonadCont m
    )
    => ((a -> cmd) -> m ()) -> m a
concludeM = concurM

-- inquire :: (MonadState s m) => t (ContT () (State (DL.DList cmd))) () -> t Identity ()
-- inquire = undefined

-- conclude :: (AsFacet (c cmd) cmd, AsFacet [cmd] cmd) => ((a -> cmd) -> c cmd`) -> ContT () (Strict.State (DL.DList cmd)) a
-- conclude m = ContT $ \k -> postcmd' $ m (codifybak . k)
-- codifybak :: AsFacet [cmd] cmd => Strict.State (DL.DList cmd) () -> cmd
-- codifybak = command' @[] . DL.toList . (`Strict.execState` mempty)

----------------------------------------------
-- Batch independant commands
----------------------------------------------

type AsConcur cmd = (AsFacet (ConcurCmd cmd) cmd)
type ConcurCmd cmd = Cmd (Concur cmd) cmd

-- | This monad is intended to be used with @ApplicativeDo@ to allow do notation
-- for composing commands that can be run concurrently.
-- The 'Applicative' instance can merge multiple commands into the internal state of @DList c@.
-- The 'Monad' instance creates a 'ConcurCmd' command before continuing the bind.
newtype Concur cmd a = Concur
    -- The base IO doesn't block (only does newEmptyMVar), but the returns an IO that blocks.
    { runConcur :: Strict.StateT (DL.DList cmd) MkMVar (IO a)
    }

instance Show (Concur cmd a) where
    showsPrec _ _ = showString "Concur"

-- | NB. Don't export MkMVar constructor to guarantee
-- that that it only contains non-blocking 'newEmptyMVar' IO.
newtype MkMVar a = MkMVar (IO a)
    deriving (Functor, Applicative, Monad)

unMkMVar :: MkMVar a -> IO a
unMkMVar (MkMVar m) = m

-- | Allows usages  of 'conclude' inside a 'concurringly' block.
-- This results in a command that requires a handler, which may be used by 'concur' or 'conclude'
concurringly :: Concur cmd a -> (a -> cmd) -> ConcurCmd cmd
concurringly = Cmd

-- | Allows usages  of 'conclude' inside a 'concurring' block.
-- This results in a command that doesn't require a handler and may be 'postcmd''ed.
concurringly_ :: Concur cmd () -> ConcurCmd cmd
concurringly_ = Cmd_

-- concurringly_2 :: (MonadTrans t, Monad (t Identity)) => t (Concur cmd) () -> t Identity (ConcurCmd cmd)
-- concurringly_2 = postcmd' . concurringly_

-- concurringly2 :: (AsConcur cmd, MonadState (DL.DList cmd) m) => t (Concur cmd) a -> (a -> cmd) -> t (Identity) ()
-- concurringly2 m = postcmd' . concurringly m



instance (AsConcur cmd) => MonadState (DL.DList cmd) (Concur cmd) where
    state m = Concur $ pure <$> Strict.state m

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
        postcmd' $ concurringly (Concur $ pure m')
            (\a -> command' $ concurringly (k a)
                (\b -> command' $ concurringly_ (Concur $ pure $ putMVar v b)))
        pure $ takeMVar v

instance AsConcur cmd => Codify (Concur cmd) cmd where
    codify f = pure $ command' . concurringly_ . f

-- | This instance makes usages of 'conclude' concurrent when used
-- insdie a 'concurringly' or 'concurringly_' block.
-- Converts a command that requires a handler to a Concur monad
-- so that the do notation can be used to compose the handler for that command.
-- The Concur monad allows scheduling the command in concurrently with other commands.
instance AsConcur cmd => MonadDelegate () (Concur cmd) where
    delegate f = Concur $ do
        v <- lift $ MkMVar newEmptyMVar
        io <- runConcur $ f (\a -> Concur $ pure $ putMVar v a)
        pure $ (io *> takeMVar v)

-- | This converts a command that requires a handler into a monad so that the do notation
-- can be used to compose the handler for that command.
-- 'concur' is used inside an 'evalContT' block or 'concurringly'.
-- If it is inside a 'evalContT' then the command is evaluated sequentially.
-- If it is inside a 'concurringly', then the command is evaluated concurrently
-- with other commands.
concur2 ::
    ( MonadDelegate () m
    , MonadState (DL.DList cmd) m
    , Codify m cmd
    , AsFacet (c cmd) cmd
    )
    => ((a -> cmd) -> c cmd) -> m a
concur2 m = concurM (postcmd' . m)

concurM ::
    ( MonadDelegate () m
    , Codify m cmd)
    => ((a -> cmd) -> m ()) -> m a
concurM m = delegate $ \k -> do
    f <- codify k
    m f
-- FIXME: concur2 is not the same as concur
-- due to Concur do monad always wrapping MVar

-- The Concur monad allows schedule the command in concurrently with other 'concur'red commands.
-- 'concur' is used inside an 'concurringly' or 'concurringly_' block.
concur :: (AsConcur cmd, AsFacet (c cmd) cmd) => ((a -> cmd) -> c cmd) -> Concur cmd a
concur k = Concur $ do
    v <- lift $ MkMVar newEmptyMVar
    postcmd' $ k (\a -> command' $ concurringly_ (Concur $ pure $ putMVar v a))
    pure $ takeMVar v
