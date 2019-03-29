{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Glazier.DebugIO where

#ifdef DEBUGIO

import Control.Monad.Delegate
import Data.Diverse.Lens
import GHC.Stack
import Glazier.Command
import Glazier.Logger

type AsDebugIO c =
    ( AsFacet [c] c -- implicity required by 'MonadCodify'
    , AsFacet (DebugIO c) c
    )

instance Show (DebugIO c) where
    showsPrec _ (DebugIO _ ) = showString "DebugIO"

-- | run arbitrary IO, should only be used for debugging
newtype DebugIO c = DebugIO (IO c)


-- -- | Variation of 'debugIO_' where the IO actino returns
-- -- the next command to process
-- -- If DEBUGIO is not defined, then this does nothing.
-- debugIO :: (HasCallStack, MonadCommand c m, AsDebugIO c) => IO c -> m ()
-- #ifdef DEBUGIO
-- debugIO m = tracedExec' callStack $ DebugIO m
-- #else
-- debugIO _ = pure ()
-- #endif

-- -- | Run an arbitrary IO. This should only be used for testing.
-- -- If DEBUGIO is not defined, then this does nothing.
-- debugIO_ :: (HasCallStack, MonadCommand c m, AsDebugIO c) => IO () -> m ()
-- #ifdef DEBUGIO
-- debugIO_ m = tracedExec' callStack $ DebugIO (command_ <$> m)
-- #else
-- debugIO_ _ = pure ()
-- #endif

-- -- | Variation of 'debugIO' where the IO action returns
-- -- the next comamand to process.
-- -- If DEBUGIO is not defined, then this does nothing.
-- debugIOThen :: (HasCallStack, AsDebugIO c, MonadCommand c m) => IO (m a) -> m a
-- #ifdef DEBUGIO
-- debugIOThen m =
--     delegate $ \fire -> do
--         -- f :: n a -> m ()
--         let f n = n >>= fire
--         -- f' :: m a -> c
--         f' <- codify f
--         tracedExec' callStack $ DebugIO (f' <$> m)
-- #else
-- debugIOThen _ = finish (pure ())
-- #endif

{-# WARNING debugIO, debugIO_, debugIOThen "Use this for debugging only. It will be disabled when DEBUGIO, ie cabal flag(debug), is not set" #-}

#endif
