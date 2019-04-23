{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.Benign.Internal where

import Control.Applicative
import Control.Monad.Morph
import qualified GHC.Generics as G

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

-- | A wrapper to indicate that the enclosed monad is non-blocking and benign.
-- Running 'Benign' effect multiple times should have no noticeable extra effects.
-- The only side effect of a Benign is it may keep a reference to an IORef
-- preventing garbage collection if you keep the Benign in scope.
newtype Benign m a = Benign (m a)
    deriving (G.Generic, G.Generic1, Functor, Applicative, Monad)

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
