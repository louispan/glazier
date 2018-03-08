{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ < 802
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Glazier.Core.Method where

import Control.Monad.Trans.Delegate
import Control.Monad.Trans.Readr

type MethodT r m a = ReadrT r (DelegateT m) a

-- pattern Method' :: ((a -> m ()) -> m ()) -> DelegateT m a
-- pattern Method' f = DelegateT (ContT f)

-- #if __GLASGOW_HASKELL__ >= 802
-- {-# COMPLETE ReadrT_ #-}
-- #endif

-- methodT' :: (r -> DelegateT m a) -> MethodT r m a
-- methodT' = readrT'

-- runMethodT' :: MethodT r m a -> (r -> DelegateT m a)
-- runMethodT' = runReadrT'

methodT' :: (r -> (a -> m ()) -> m ()) -> MethodT r m a
methodT' = readrT' . (delegateT' .)

runMethodT' :: MethodT r m a -> (r -> (a -> m ()) -> m ())
runMethodT' = (runDelegateT' .) . runReadrT'

