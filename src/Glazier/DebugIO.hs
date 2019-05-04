{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.DebugIO where

#ifdef DEBUGIO

import Data.Diverse.Lens
import GHC.Stack
import Glazier.Logger

type AsDebugIO c = AsFacet (DebugIO c) c

instance Show (DebugIO c) where
    showsPrec _ (DebugIO _ ) = showString "DebugIO"

-- | run arbitrary IO, should only be used for debugging
newtype DebugIO c = DebugIO (IO c)
    deriving (Functor, Applicative, Monad)

-- -- | Variation of 'debugIO_' where the IO action returns
-- -- the next command to process
-- -- If DEBUGIO is not defined, then this does nothing.
-- execDebugIO :: (HasCallStack, MonadCommand c m, AsLogger c, AsDebugIO c) => IO c -> m ()
-- #ifdef DEBUGIO
-- execDebugIO m = logExec' Trace callStack $ DebugIO m
-- #else
-- execDebugIO _ = pure ()
-- #endif

-- | Run an arbitrary IO. This should only be used for testing.
-- If DEBUGIO is not defined, then this does nothing.
debugIO :: (HasCallStack, AsDebugIO c, AsFacet LogLine c, MonadCommand c m, AskLogLevel m) => IO a -> m a
#ifdef DEBUGIO
debugIO m = logInvoke TRACE callStack $ DebugIO m
#else
debugIO _ = finish (pure ())
#endif

-- -- | Variation of 'debugIO' where the IO action returns
-- -- the next monad action to process.
-- -- If DEBUGIO is not defined, then this does nothing.
-- debugIOThen :: (HasCallStack, AsDebugIO c, AsLogger c, MonadCommand c m) => IO (m a) -> m a
-- #ifdef DEBUGIO
-- debugIOThen = join . debugIO
-- #else
-- debugIOThen _ = finish (pure ())
-- #endif

{-# WARNING debugIO "Use this for debugging only. It will be disabled when DEBUGIO, ie cabal flag(debug), is not set" #-}

#endif
