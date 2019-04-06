{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.Benign.Internal where

import Control.Applicative
import Control.Monad.Morph

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

-- | A wrapper to indicate that the enclosed monad is non-blocking and benign.
newtype Benign m a = Benign (m a)
    deriving (Functor, Applicative, Monad)

instance MonadTrans Benign where
    lift = Benign

instance MFunctor Benign where
    hoist f (Benign m) = Benign (f m)

instance MMonad Benign where
    embed f (Benign m) = f m

instance (Semigroup a, Applicative m) => Semigroup (Benign m a) where
    Benign f <> Benign g = Benign $ liftA2 (<>) f g

instance (Monoid a, Applicative m) => Monoid (Benign m a) where
    mempty = Benign $ pure mempty
#if !MIN_VERSION_base(4,11,0)
    (Benign f) `mappend` (Benign g) = Benign (liftA2 mappend f g)
#endif

getBenign :: Benign m a -> m a
getBenign (Benign m) = m
