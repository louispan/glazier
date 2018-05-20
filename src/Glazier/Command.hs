{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
    ( Cmd(..)
    , MonadCodify(..)
    , codify'
    , MonadCommand
    , command
    , command'
    , post
    , postcmd
    , postcmd'
    , dispatch
    , conclude
    , concurringly
    , concurringly_
    , AsConcur
    , ConcurCmd
    , Concur(..)
    , MkMVar -- Hiding constructor
    , unMkMVar
    , concurCmd
    , concurCmd_
    ) where

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
import Control.Monad.Trans.Maybe
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

instance Show (Cmd f cmd) where
    showsPrec p (Cmd f _) = showParen (p >= 11) $
        showString "Cmd " . shows f

-- | Converts a monad transformer stack with a 'State' of list of commands
-- to output a single command, using the current monad context,
-- by running the State of comands with mempty like Writer.
class Monad m => MonadCodify cmd m | m -> cmd where
    codify :: (a -> m ()) -> m (a -> cmd)

codify' :: MonadCodify cmd m => m () -> m cmd
codify' m = do
    f <- codify (const m)
    pure (f ())

instance AsFacet [cmd] cmd => MonadCodify cmd (Strict.State (DL.DList cmd)) where
    codify f = pure $ \a -> case DL.toList . (`Strict.execState` mempty) $ f a of
        [x] -> x
        xs -> command' xs

instance AsFacet [cmd] cmd => MonadCodify cmd (Strict.AState (DL.DList cmd)) where
    codify f = pure $ \a -> case DL.toList . (`Strict.execAState` mempty) $ f a of
        [x] -> x
        xs -> command' xs

instance AsFacet [cmd] cmd => MonadCodify cmd (Lazy.State (DL.DList cmd)) where
    codify f = pure $ \a -> case DL.toList . (`Lazy.execState` mempty) $ f a of
        [x] -> x
        xs -> command' xs

instance AsFacet [cmd] cmd => MonadCodify cmd (Lazy.AState (DL.DList cmd)) where
    codify f = pure $ \a -> case DL.toList . (`Lazy.execAState` mempty) $ f a of
        [x] -> x
        xs -> command' xs

instance (MonadCodify cmd m, Monad m) => MonadCodify cmd (IdentityT m) where
    codify f = lift . codify $ runIdentityT . f

instance (MonadCodify cmd m, Monad m) => MonadCodify cmd (ContT () m) where
    codify f = lift . codify $ evalContT . f

instance (MonadCodify cmd m, Monad m) => MonadCodify cmd (AContT () m) where
    codify f = lift . codify $ evalAContT . f

instance (MonadCodify cmd m, Monad m) => MonadCodify cmd (ReaderT r m) where
    codify f = do
        r <- ask
        lift . codify $ (`runReaderT` r) . f

instance (MonadCodify cmd m, Monad m) => MonadCodify cmd (AReaderT r m) where
    codify f = do
        r <- ask
        lift . codify $ (`runAReaderT` r) . f

instance (MonadCodify cmd m, Monad m) => MonadCodify cmd (MaybeT m) where
    codify f = lift . codify $ void . runMaybeT . f

-- instance (AsFacet e cmd, MonadCodify cmd m, Monad m, MonadState (DL.DList cmd) m) => MonadCodify cmd (ExceptT e m) where
--     codify f = lift . codify $ go . runExceptT . f
--       where
--         go m = do
--             ea <- m
--             case ea of
--                 Left e -> postcmd e
--                 Right () -> pure ()

type MonadCommand cmd m =
    ( MonadDelegate () m
    , MonadCodify cmd m
    , MonadState (DL.DList cmd) m
    )

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

-- | Add a command to the list of commands for this MonadState.
-- I basically want a Writer monad, but I'm using a State monad
-- because but I also want to use it inside a ContT which only has an instance of MonadState.
post :: (MonadState (DL.DList cmd) m) => cmd -> m ()
post c = id %= (`DL.snoc` c)

-- | @'postcmd' = 'post' . 'command'@
postcmd :: (MonadState (DL.DList cmd) m, AsFacet c cmd) => c -> m ()
postcmd = post . command

-- | @'postcmd'' = 'post' . 'command''@
postcmd' :: (MonadState (DL.DList cmd) m, AsFacet (c cmd) cmd) => c cmd -> m ()
postcmd' = post . command'

-- | This converts a monadic function that requires a handler for @a@ into
-- a monad that fires the @a@ so that the do notation can be used to compose the handler.
-- 'dispatch' is used inside an 'evalContT' block or 'concurringly'.
-- If it is inside a 'evalContT' then the command is evaluated sequentially.
-- If it is inside a 'concurringly', then the command is evaluated concurrently
-- with other commands.
--
-- @
-- If tne input function purely returns a command, you can use:
-- dispatch . (postcmd' .) :: ((a -> cmd) -> c cmd) -> m a
--
-- If tne input function monnadic returns a command, you can use:
-- dispatch . ((>>= postcmd') .) :: ((a -> cmd) -> m (c cmd)) -> m a
-- @
dispatch ::
    ( MonadCommand cmd m) -- NB. @MonadState (DL.DList cmd) m@ is redundant
    => ((a -> cmd) -> m ()) -> m a
dispatch m = delegate $ \k -> do
    f <- codify k
    m f

-- | Sequential variation of 'dispatch' that forces the transformer stack to use 'ContT'.
-- The 'MonadCont' constraint is redundant but rules out
-- using 'Concur' at the bottom of the transformer stack.
-- 'conclude' is used for operations that MUST run sequentially, not concurrently.
conclude ::
    ( MonadCommand cmd m
    , MonadCont m
    )
    => ((a -> cmd) -> m ()) -> m a
conclude = dispatch

----------------------------------------------
-- Batch independant commands
----------------------------------------------

type AsConcur cmd = (AsFacet () cmd, AsFacet (ConcurCmd cmd) cmd)
type ConcurCmd cmd = Cmd (Concur cmd) cmd

-- | This monad is intended to be used with @ApplicativeDo@ to allow do notation
-- for composing commands that can be run concurrently.
-- The 'Applicative' instance can merge multiple commands into the internal state of @DList c@.
-- The 'Monad' instance creates a 'ConcurCmd' command before continuing the bind.
newtype Concur cmd a = Concur
    -- The base IO doesn't block (only does newEmptyMVar), but may return an IO that blocks.
    -- The return is @Either (IO a) a@ where 'Left' is used for blocking IO
    -- and 'Right' is used for nonblocking pure values.
    -- This distinction prevents nested layers of MVar for pure monadic binds.
    -- See the instance of 'Monad' for 'Concur'.
    -- Once a blocking IO is returned, then all subsequent binds require another nested MVar.
    -- So it is more efficient to groups of pure binds first before binding with blocking code.
    { runConcur :: Strict.StateT (DL.DList cmd) MkMVar (Either (IO a) a)
    }

instance Show (Concur cmd a) where
    showsPrec _ _ = showString "Concur"

-- | NB. Don't export MkMVar constructor to guarantee
-- that that it only contains non-blocking 'newEmptyMVar' IO.
newtype MkMVar a = MkMVar (IO a)
    deriving (Functor, Applicative, Monad)

unMkMVar :: MkMVar a -> IO a
unMkMVar (MkMVar m) = m

-- | 'dispatch' can be used inside a 'concurCmd' or 'concurringly' block.
-- 'concurCmd' results in a function (that requires a handler) which may be passed into 'conclude''
concurCmd :: Concur cmd a -> (a -> cmd) -> ConcurCmd cmd
concurCmd = Cmd

-- | 'dispatch' can be used inside a 'concurCmd_' or 'concurringly_' block.
-- 'concurCmd_' results in a command that doesn't require a handler and may be passed into 'postcmd''.
concurCmd_ :: AsFacet () cmd => Concur cmd () -> ConcurCmd cmd
concurCmd_ m = Cmd m command

-- This is a monad morphism that can be used to 'Control.Monad.Morph.hoist' transformer stacks on @Concur cmd a@
concurringly ::
    ( MonadCommand cmd m
    , AsConcur cmd
    , MonadCont m
    ) => Concur cmd a -> m a
concurringly m = conclude $ postcmd' . (concurCmd m)

-- This is a monad morphism that can be used to 'Control.Monad.Morph.hoist' transformer stacks on @Concur cmd ()@
concurringly_ :: (MonadState (DL.DList cmd) m, AsConcur cmd) => Concur cmd () -> m ()
concurringly_ = postcmd' . concurCmd_

instance (AsConcur cmd) => MonadState (DL.DList cmd) (Concur cmd) where
    state m = Concur $ Right <$> Strict.state m

instance Functor (Concur cmd) where
    fmap f (Concur m) = Concur $ (either (Left . fmap f) (Right . f)) <$> m

-- | Applicative instand allows building up list of commands without blocking
instance Applicative (Concur cmd) where
    pure = Concur . pure . pure
    (Concur f) <*> (Concur a) = Concur $ liftA2 go f a
      where
        go :: Either (IO (a -> b)) (a -> b)
             -> Either (IO a) a
             -> Either (IO b) b
        go g b = case (g, b) of
            (Left g', Left b') -> Left (g' <*> b')
            (Left g', Right b') -> Left (($b') <$> g')
            (Right g', Left b') -> Left (g' <$> b')
            (Right g', Right b') -> Right (g' b')

-- Monad instance can't build commands without blocking.
instance (AsConcur cmd) => Monad (Concur cmd) where
    (Concur m) >>= k = Concur $ do
        m' <- m -- get the blocking io action while updating the state
        case m' of
            -- pure value, no blocking required,
            -- avoid using MVar.
            Right a -> runConcur $ k a
            Left ma -> do
                v <- lift $ MkMVar newEmptyMVar
                postcmd' $ concurCmd (Concur $ pure (Left ma))
                    (\a -> command' $ concurCmd (k a)
                        (\b -> command' $ concurCmd_ (Concur $ pure $ Left $ putMVar v b)))
                pure $ Left $ takeMVar v

instance AsConcur cmd => MonadCodify cmd (Concur cmd) where
    codify f = pure $ command' . concurCmd_ . f

-- | This instance makes usages of 'conclude' concurrent when used
-- insdie a 'concurringly' or 'concurringly_' block.
-- Converts a command that requires a handler to a Concur monad
-- so that the do notation can be used to compose the handler for that command.
-- The Concur monad allows scheduling the command in concurrently with other commands.
instance AsConcur cmd => MonadDelegate () (Concur cmd) where
    delegate f = Concur $ do
        v <- lift $ MkMVar newEmptyMVar
        b <- runConcur $ f (\a -> Concur $ lift $ pure $ Left $ putMVar v a)
        pure $ Left (either id pure b *> takeMVar v)
