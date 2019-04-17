{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.Command
    ( MonadCodify(..)
    , codify'
    , ProgramT(..)
    , Program
    , programT'
    , runProgramT'
    , MonadProgram(..)
    , MonadCommand
    , command
    , command'
    , command_
    , commands
    , exec
    , exec'
    , delegatify
    , eval
    , eval'
    , invoke
    , invoke_
    , sequentially
    , concurringly
    , concurringly_
    , AsConcur
    , Concur(..)
    , NewChanIO
    -- | NB. Don't export NewChanIO constructor to guarantee
    -- that that it only contains non-blocking 'newEmptyMVar' IO.
    , newChanIO
    , unNewChanIO
    ) where

import Control.Also
import Control.Applicative
import Control.Concurrent
import Control.Lens
import Control.Monad.Cont
import Control.Monad.Delegate
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Data.Diverse.Lens
import qualified Data.DList as DL
import qualified GHC.Generics as G
import Glazier.Command.Internal
import Data.Semigroup

----------------------------------------------
-- Command utilties
----------------------------------------------

-- | convert a request type to a command type.
-- This is used for commands that doesn't have a continuation.
-- Ie. commands that doesn't "returns" a value from running an effect.
-- Use 'command'' for commands that require a continuation ("returns" a value).
command :: (AsFacet cmd c) => cmd -> c
command = review facet

-- | A variation of 'command' for commands with a type variable @c@,
-- which is usually commands that are containers of command,
-- or commands that require a continuation
-- Eg. commands that "returns" a value from running an effect.
command' :: (AsFacet (cmd c) c) => cmd c -> c
command' = review facet

-- | This function is useful for 'fmap' a @cmd ()@ into @cmd c@.
-- A variation of 'command' specific for unit that uses a
-- @AsFacet [c] c@ constraint instead of
-- @AsFacet () c@ to avoid polluting the contstraints
-- when 'commands' is used.
command_ :: (AsFacet [c] c) => () -> c
command_ = command' . \() -> []

-- | Convert a list of commands to a command.
-- This implementation avoids nesting for lists of a single command.
commands :: (AsFacet [c] c) => [c] -> c
commands [x] = x
commands xs = command' xs

-- | Converts a handler that result in 'MonadProgram'
-- to a handler that result in a command, using the current monad context.
-- This is used to extract out the command in a 'MonadProgram'.
class Monad m => MonadCodify c m | m -> c where
    codify :: (a -> m ()) -> m (a -> c)

-- | Variation of 'codify' to transform the monad stack instead of a handler.
codify' :: (MonadCodify c m) => m () -> m c
codify' m = do
    f <- codify (const m)
    pure (f ())

-- | A monad transformer with a instance of 'MonadProgram'.
-- Although this is a transfromer, it does not "passthrough" instance of
-- 'MonadReader', etc from the inner monad.
-- It is a CPS WriterT (ie, StateT) of list of commands.
-- Using it as a base monad @Program c@ gives an instance of 'MonadCodify'.
-- This is typically used in a transformer stack with @ContT ()@ for an instance of 'MonadDelegate'
-- for sequentially building up commands with return values to handle.
-- 'Concur', a newtype of @Strict.StateT (DL.DList c) NewEmptyMVar x@ is also a instance of 'MonadCodify',
-- 'Concur' has a instance of 'MonadDelegate' which allows concurrent evaluation of
-- commands with return values to handle.
newtype ProgramT c m a = ProgramT { runProgramT :: Strict.StateT (DL.DList c) m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MFunctor, MonadIO)

type Program c = ProgramT c Identity

-- | Passthrough instance
instance (Also m a, Monad m) => Also (ProgramT c m) a where
    alsoZero = lift alsoZero
    ProgramT f `also` ProgramT g = ProgramT $ f `also` g

-- | Passthrough instance
instance (Monoid (m a), Monad m) => Monoid (ProgramT c m a) where
    mempty = lift mempty
#if !MIN_VERSION_base(4,11,0)
    -- follow the `also` instance of 'StateT'
    (ProgramT f) `mappend` (ProgramT g) = ProgramT $ do
        (x, y) <- liftA2 (,) f g
        lift $ pure x `mappend` pure y
#endif

-- | Passthrough instance
instance (Semigroup (m a), Monad m) => Semigroup (ProgramT c m a) where
    -- follow the `also` instance of 'StateT'
    (ProgramT f) <> (ProgramT g) = ProgramT $ do
        (x, y) <- liftA2 (,) f g
        lift $ pure x <> pure y

programT' :: (DL.DList c -> m (a, DL.DList c)) -> ProgramT c m a
programT' = ProgramT . Strict.StateT

runProgramT' :: ProgramT c m a -> DL.DList c -> m (a, DL.DList c)
runProgramT' = Strict.runStateT . runProgramT

-- | Monad that can be added instructions of commands.
-- To extract the command see 'MonadCodify'
class Monad m => MonadProgram c m | m -> c where
    -- | Add a command to the program
    instruct :: c -> m ()

-- | Instance that does real work by running the State of commands with mempty.
-- Essentially a Writer monad, but using a State monad so it can be
-- used inside a ContT which only has an instance for MonadState.
instance AsFacet [c] c => MonadCodify c (Program c) where
    codify f = pure $ commands . DL.toList . (`Strict.execState` mempty) . runProgramT . f

instance Monad m => MonadProgram c (ProgramT c m) where
    instruct c = ProgramT $ Strict.modify' (`DL.snoc` c)

-- | Passthrough instance
instance (MonadCodify c m) => MonadCodify c (Strict.StateT s m) where
    codify f = do
        s <- Strict.get
        lift $ codify $ \a -> (`Strict.evalStateT` s) $ f a

instance MonadProgram c m => MonadProgram c (Strict.StateT s m) where
    instruct = lift . instruct

-- | Passthrough instance
instance MonadCodify c m => MonadCodify c (Lazy.StateT s m) where
    codify f = do
        s <- Lazy.get
        lift $ codify $ \a -> (`Lazy.evalStateT` s) $ f a

instance MonadProgram c m => MonadProgram c (Lazy.StateT s m) where
    instruct = lift . instruct

-- | Passthrough instance
instance MonadCodify c m => MonadCodify c (IdentityT m) where
    codify f = lift . codify $ runIdentityT . f

instance MonadProgram c m => MonadProgram c (IdentityT m) where
    instruct = lift . instruct

-- | Passthrough instance
instance MonadCodify c m => MonadCodify c (ContT () m) where
    codify f = lift . codify $ evalContT . f

instance MonadProgram c m => MonadProgram c (ContT a m) where
    instruct = lift . instruct

-- | Passthrough instance, using the Reader context
instance MonadCodify c m => MonadCodify c (ReaderT r m) where
    codify f = do
        r <- ask
        lift . codify $ (`runReaderT` r) . f

instance MonadProgram c m => MonadProgram c (ReaderT r m) where
    instruct = lift . instruct

instance (Monoid w, MonadCodify c m) => MonadCodify c (Lazy.WriterT w m) where
    codify f = lift . codify $ fmap fst . Lazy.runWriterT . f

instance (Monoid w, MonadProgram c m) => MonadProgram c (Lazy.WriterT w m) where
    instruct = lift . instruct

instance (Monoid w, MonadCodify c m) => MonadCodify c (Strict.WriterT w m) where
    codify f = lift . codify $ fmap fst . Strict.runWriterT . f

instance (Monoid w, MonadProgram c m) => MonadProgram c (Strict.WriterT w m) where
    instruct = lift . instruct

-- | Passthrough instance, ignoring that the handler result might be Nothing.
instance MonadCodify c m => MonadCodify c (MaybeT m) where
    codify f = lift . codify $ void . runMaybeT . f

instance MonadProgram c m => MonadProgram c (MaybeT m) where
    instruct = lift . instruct

-- | Passthrough instance which requires the inner monad to be a 'MonadDelegate'.
-- This means that the @Left e@ case can be handled by the provided delegate.
instance (MonadDelegate m, MonadCodify c m) => MonadCodify c (ExceptT e m) where
    codify f = ExceptT $ delegate $ \kec -> do
        let g a = do
                e <- runExceptT $ f a
                case e of
                    Left e' -> kec (Left e')
                    Right _ -> pure ()
        g' <- codify g
        kec (Right g')

instance MonadProgram c m => MonadProgram c (ExceptT e m) where
    instruct = lift . instruct

type MonadCommand c m =
    ( MonadProgram c m
    , MonadDelegate m
    , MonadCodify c m
    -- , AsFacet [c] c
    )

-- | @'exec' = 'instruct' . 'command'@
--
-- 'exec', 'exec'', 'defer', 'eval', 'eval'', 'eval'''
-- can be used inside an 'evalContT' or 'concurringly'.
-- If it is inside a 'evalContT' then the command is evaluated sequentially.
-- If it is inside a 'concurringly', then the command is evaluated concurrently
-- with other commands.
exec :: (MonadProgram c m, AsFacet cmd c) => cmd -> m ()
exec = instruct . command

-- | @'exec'' = 'instruct' . 'command''@
exec' :: (MonadProgram c m, AsFacet (cmd c) c) => cmd c -> m ()
exec' = instruct . command'

-- | Uses 'delegate' and 'codify' together.
-- Uses 'delegate' to get the @a -> m()@ handler
-- and 'codify' to convert the handler to a command form @a -> c@.
-- which is the pass to a block of code to a convert a monad that fires an @a@.
--
-- That is, this converts a function that returns a monad and requires a handler for @a@ into
-- a monad that fires the @a@ so that the do notation can be used to compose the handler.
-- @
--
-- contrast this type with @'delegate' :: ((a -> m ()) -> m ()) -> m a@
delegatify ::
    ( MonadDelegate m
    , MonadCodify c m
    )
    => ((a -> c) -> m ()) -> m a
delegatify m = delegate $ \k -> do
    f <- codify k
    m f

-- | Convert a command that needs a handler for @a@
-- into a `MonadCommand` that fires @a@
eval ::
    ( MonadCommand c m
    , AsFacet cmd c
    )
    => ((a -> c) -> cmd) -> m a
eval k = delegatify $ exec . k

-- | Convert a command that needs a handler for @a@
-- into a `MonadCommand` that fires @a@
eval' ::
    ( MonadCommand c m
    , AsFacet (cmd c) c
    )
    => ((a -> c) -> cmd c) -> m a
eval' k = delegatify $ exec' . k

-- | Convert a functor into a 'MonadCommand' that fires an @a@
invoke ::
    ( MonadCommand c m
    , AsFacet (cmd c) c
    , Functor cmd
    )
    => cmd a -> m a
invoke c = eval' (<$> c)

-- A simpler variation of 'invoke' that only requires a @MonadProgram c m@
invoke_ ::
    ( MonadProgram c m
    , AsFacet (cmd c) c
    , AsFacet [c] c
    , Functor cmd
    )
    => cmd () -> m ()
invoke_ = exec' . fmap command_

-- | Adds a 'MonadCont' constraint. It is redundant but rules out
-- using 'Concur' at the bottom of the transformer stack,
-- which prevents the use of `concurringly`.
-- 'sequentially' is used for operations that MUST run sequentially, not concurrently.
-- Eg. when the overhead of using 'Concur' 'MVar' is not worth it, or
-- when data dependencies are not explicitly specified by monadic binds,
-- Eg. A command to update mutable variable must execute before
-- a command that reads from the mutable variable.
-- In this case, the reference to the variable doesn't change, so the
-- data dependency is not explicit.
sequentially :: MonadCont m => m a -> m a
sequentially = id

----------------------------------------------
-- Batch independant commands
----------------------------------------------

type AsConcur c = (AsFacet [c] c, AsFacet (Concur c c) c)

-- | This monad is intended to be used with @ApplicativeDo@ to allow do notation
-- for composing commands that can be run concurrently.
-- The 'Applicative' instance can merge multiple commands into the internal state of @DList c@.
-- The 'Monad' instance creates a 'ConcurCmd' command before continuing the bind.
newtype Concur c a = Concur
    -- The base IO doesn't block (only does newChan), but may return an IO that blocks.
    -- The return is @Either (IO a) a@ where 'Left' is used for blocking IO
    -- that could potentially fail with BlockedIndefinitelyOnMVar.
    -- Tje 'Right' is used for nonblocking pure values.
    -- This distinction prevents nested layers of Chan for monadic binds with pure Right values.
    -- See the instance of 'Monad' for 'Concur'.
    -- Once a Left blocking IO is returned, then all subsequent binds require another nested MVar.
    -- So it is more efficient to groups of pure binds first before binding with blocking code.
    -- The interpreter 'Glazier.Command.Exec.execConcur' will run any Left IO action
    -- until failure with BlockedIndefinitelyOnMVar to drain all the values from the Chan.
    -- DNAGER: So do not put anything in Left IO that will never eventually fail!
    { runConcur :: Strict.StateT (DL.DList c) NewChanIO (Either (IO a) a)
    } deriving (G.Generic)

instance Show (Concur c a) where
    showsPrec _ _ = showString "Concur"

-- This is a monad morphism that can be used to 'Control.Monad.Morph.hoist' transformer stacks on @Concur c a@
-- This is 'invoke' type constrained to @Concur c a@
concurringly ::
    ( MonadCommand c m
    , AsConcur c
    ) => Concur c a -> m a
concurringly = invoke

-- | This is a monad morphism that can be used to 'Control.Monad.Morph.hoist' transformer stacks on @Concur c ()@
-- A simpler variation of 'concurringly' that only requires a @MonadProgram c m@
-- This is 'invoke_' type constrained to @Concur c ()@
concurringly_ :: (MonadProgram c m, AsConcur c) => Concur c () -> m ()
concurringly_ = invoke_

instance Functor (Concur c) where
    fmap f (Concur m) = Concur $ (either (Left . fmap f) (Right . f)) <$> m

-- | Applicative instand allows building up list of commands without blocking
instance Applicative (Concur c) where
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
instance (AsConcur c) => Monad (Concur c) where
    (Concur m) >>= k = Concur $ do
        m' <- m -- get the blocking io action while updating the state
        case m' of
            -- pure value, no blocking required, avoid using Chan.
            Right a -> runConcur $ k a
            -- blocking io, must use Chan
            -- We use chan because the monad may fire into the delegate more than once
            Left ma -> do
                v <- lift $ newChanIO
                -- Aim: convert the Concur IO effects into a command to be interpreted
                -- first, wrap the @ma@ we want to run into a Concur, then fmap it
                -- to convert @Concur c a@ to @Concur c c@
                runProgramT $ exec' $ flip fmap (Concur @c $ pure (Left ma))
                    -- goal :: a -> c
                    -- Given the result of @a@, run through the bind @k@
                    -- to get the resultant @Concur c b@, then fmap it
                    -- to convert @Concur c b@ to @Concur c c@
                    -- then command' converts @Concur c c@ to a @c@
                    (\a -> command' $ flip fmap (k a)
                        -- goal :: b -> c
                        -- Given @b@ result @Concur c b@ from the bind,
                        -- 'writeChan' the @b@ of bind into @v@
                        (\b -> command' -- command' converts @Concur c c@ to a @c@
                            -- command_ converts @Concur c ()@ to @Concur c c@
                            $ command_ <$> (Concur @c $ pure $ Left $ writeChan v b)))
                -- return the IO effect to "fire" @a@ into the Chan
                -- FIXME: protect this against BlockedIndefinitelyOnMVar?
                pure $ Left $ readChan v

-- | The monad @Concur c c@ itself is a command @c@, so an instance of @MonadCodify@ canbe made
instance AsConcur c => MonadCodify c (Concur c) where
    codify f = pure $
         command' -- a -> c
         . fmap command_ -- a -> Concur c c
         . f -- a -> Concur c ()

instance AsConcur c => MonadProgram c (Concur c) where
    instruct c = Concur $ Right <$> Strict.modify' (`DL.snoc` c)

-- | This instance makes usages of 'eval'' concurrent when used
-- inside a 'concurringly' or 'concurringly_' block.
-- Converts a command that requires a handler to a Concur monad
-- so that the do notation can be used to compose the handler for that command.
-- The Concur monad allows scheduling the command in concurrently with other commands.
instance AsConcur c => MonadDelegate (Concur c) where
    delegate f = Concur $ do
        v <- lift $ newChanIO
        -- e :: Either (IO ()) ()
        e <- runConcur $ f (\a -> Concur $ lift $ pure $ Left $ writeChan v a)
        -- The Left blocking action runs any IO () if necessary, and then readChan v
        pure $ Left (either id pure e *> readChan v)

-- | Passthrough instance
instance AsConcur c => Also (Concur c) a where
    -- This creates a left blocking action that will produce any values
    -- before a BlockedIndefinitelyOnMVar.
    alsoZero = finish (pure ())

    f `also` g = delegate $ \fire -> Concur $ do
        (x, y) <- liftA2 (,) (runConcur f) (runConcur g)
        case (x, y) of
            (Right x', Right y') ->
                -- purely fire both results straight awy
                (runConcur $ fire x') *> (runConcur $ fire y')
            (Right x', Left y') -> do
                -- fire the pure Right bit straight away
                void . runConcur $ fire x'
                scheduleConcur fire y'
                pure $ Right ()
            (Left x', Right y') -> do
                -- fire the pure Right bit straight away
                void . runConcur $ fire y'
                scheduleConcur fire x'
                pure $ Right ()
            (Left x', Left y') -> do
                scheduleConcur fire x'
                scheduleConcur fire y'
                pure $ Right ()
      where
        -- forkIO for the potentially blocking IO
        -- y' :: IO a
        -- Aim: convert the Concur IO effects into a command to be interpreted
        -- first, wrap the Left IO we want to run into a Concur, then fmap it
        -- to convert @Concur c a@ to @Concur c c@
        scheduleConcur fire ma = runProgramT $ exec' $ flip fmap (Concur @c $ pure (Left ma))
            -- goal :: a -> c
            -- Given the result of @a@, run through the @fire@
            -- to get the resultant @Concur c ()@, then fmap it
            -- to convert @Concur c ()@ to @Concur c c@
            -- then command' converts @Concur c c@ to a @c@
            (\a -> command' $ command_ <$> (fire a))

-- -- -- | Passthrough instance
-- -- instance Monoid (Concur c a) where
-- --     mempty = lift mempty
-- -- #if !MIN_VERSION_base(4,11,0)
-- --     -- follow the `also` instance of 'StateT'
-- --     (ProgramT f) `mappend` (ProgramT g) = ProgramT $ do
-- --         (x, y) <- liftA2 (,) f g
-- --         lift $ pure x `mappend` pure y
-- -- #endif

-- -- | Passthrough instance
-- instance Semigroup (Concur c a) where
--     -- follow the `also` instance of 'StateT'
--     (ProgramT f) <> (ProgramT g) = ProgramT $ do
--         (x, y) <- liftA2 (,) f g
--         lift $ pure x <> pure y
