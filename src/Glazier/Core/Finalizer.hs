module Glazier.Core.Finalizer where

import Control.Disposable as CD
import qualified Glazier.Core.Method as Z

-- | Unforunately, because each widget contains callbacks
-- that has to be cleaned manually, we can't just rely on the garbage collector.
-- This contains 'Disposable' to be called on the *next* render.
-- This must be called before removing widgets from a containing model.
type Finalizer s m = Z.Method s m CD.Disposable

-- -- mempty
-- memptyFinalizer :: Applicative m => Finalizer s m
-- memptyFinalizer = mempty

-- -- mappend
-- mappendFinalizer :: Applicative m => Finalizer s m -> Finalizer s m -> Finalizer s m
-- mappendFinalizer = mappend
-- infixr 6 `mappendFinalizer` -- like mappend
