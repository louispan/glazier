{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.Core.Method where

import Control.Applicative
import Control.Lens
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Zip
import Control.Newtype
import Data.Coerce
import Data.Semigroup
import qualified GHC.Generics as G

-- | A newtype wrapper around ReaderT for custom monoid instances
-- Memonic: Method ae associated with an object (the reader environment).
newtype Method r m a = Method { runMethod :: ReaderT r m a }
    deriving
    ( G.Generic
    , MonadTrans
    , Monad
    , Functor
    , MonadFix
    , MonadFail
    , Applicative
    , MonadZip
    , MonadIO
    , Alternative
    , MonadPlus
    , MonadReader r
    , MFunctor
    , MMonad
    )

instance Newtype (Method r m a)

deriving instance MonadWriter w m => MonadWriter w (Method r m)
deriving instance MonadState s m => MonadState s (Method r m)
deriving instance MonadError e m => MonadError e (Method r m)
deriving instance MonadCont m => MonadCont (Method r m)

type instance Magnified (Method r m) = Magnified (ReaderT r m)
instance Monad m => Magnify (Method s m) (Method t m) s t where
    magnify l (Method f) = Method (magnify l f)

type instance Zoomed (Method e m) = Zoomed (ReaderT e m)
instance Zoom m n s t => Zoom (Method e m) (Method e n) s t where
    zoom l (Method f) = Method (zoom l f)

-- | This is the reason for the newtye wrapper
-- This is different from the Alternative/MonadPlus instance.
-- The Alternative/MonadPlus instance runs one or the other
-- The Semigroup/Monoid instnaces runs both.
instance (Applicative m, Semigroup a) => Semigroup (Method r m a) where
    (<>) = liftA2 (<>)

-- | This is the reason for the newtye wrapper
-- This is different from the Alternative/MonadPlus instance.
-- The Alternative/MonadPlus instance runs one or the other
-- The Semigroup/Monoid instances runs both.
instance (Applicative m, Monoid a) => Monoid (Method r m a) where
    mempty = pure mempty
    mappend = liftA2 mappend

method' :: (r -> m a) -> Method r m a
method' = coerce

runMethod' :: Method r m a -> r -> m a
runMethod' = coerce
