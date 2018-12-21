{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Glazier.DebugIO where

import Control.Monad.Delegate
import Data.Diverse.Lens
import Glazier.Command

type AsDebugIO c =
    ( AsFacet [c] c -- implicity required by 'MonadCodify'
    , AsFacet (DebugIO c) c
    )

instance Show (DebugIO c) where
    showsPrec _ (DebugIO _ ) = showString "DebugIO"

#ifdef DEBUGIO
-- | run arbitrary IO, should only be used for debugging
newtype DebugIO c = DebugIO (IO c)
#else
-- | run arbitrary IO, should only be used for debugging
newtype DebugIO c = DebugIO c
#endif
