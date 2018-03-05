{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.Core.Delegate where

import Control.Applicative
import Control.Lens
import Control.Monad.Cont
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Trans.Cont.Extras as TE
import Control.Monad.Zip
import Control.Newtype
import Data.Coerce
import Data.Semigroup
import qualified GHC.Generics as G

-- | A 'Delegate' is like a 'Glazier.Core.Method' (a reader with an associated object)
-- but where some work is delegated to a handler (the ContT continuation).
newtype Delegate r m a = Delegate { runDelegate :: ReaderT r (ContT () m) a }
    deriving
    ( G.Generic
    , Monad
    , Functor
    , MonadFail
    , Applicative
    , MonadIO
    , MonadReader r
    , MonadCont
    )

instance Newtype (Delegate r m a)

deriving instance MonadState s (ContT () m) => MonadState s (Delegate r m)

instance MonadTrans (Delegate r) where
    lift = Delegate . lift . lift

instance MonadZip (Delegate r m) where
    mzip x y = liftA2 (,) x y

type instance Magnified (Delegate r m) = Magnified (ReaderT r (ContT () m))
instance Magnify (Delegate s m) (Delegate t m) s t where
    magnify l (Delegate f) = Delegate (magnify l f)

type instance Zoomed (Delegate e m) = Zoomed (ReaderT e (ContT () m))
instance Zoom (ContT () m) (ContT () n) s t => Zoom (Delegate e m) (Delegate e n) s t where
    zoom l (Delegate f) = Delegate (zoom l f)

-- | This is the reason for the newtye wrapper
-- This is different from the Alternative/MonadPlus instance.
-- The Alternative/MonadPlus instance runs one or the other
-- The Semigroup/Monoid instances runs both, and fires the output twice.
instance (Applicative m) => Semigroup (Delegate r m c) where
    Delegate (ReaderT x) <> Delegate (ReaderT y) =
        Delegate . ReaderT $ liftA2 (TE.alsoContT) x y

-- | This is the reason for the newtye wrapper
-- This is different from the Alternative/MonadPlus instance.
-- The Alternative/MonadPlus instance runs one or the other
-- The Semigroup/Monoid instances runs both, and fires the output twice.
instance (Applicative m) => Monoid (Delegate r m c) where
    mempty = Delegate . ReaderT . const . ContT . const $ pure ()
    mappend = (<>)

-- | ContT didn't have an instance of Alternative1
instance Alternative m => Alternative (Delegate r m) where
    empty = Delegate $ lift $ ContT $ const empty
    Delegate (ReaderT x) <|> Delegate (ReaderT y) =
        Delegate $ ReaderT $ \r -> ContT $ \k ->
            runContT (x r) k <|> runContT (y r) k

instance MonadPlus m => MonadPlus (Delegate r m) where
    mzero = empty
    mplus = (<|>)

delegate' :: (r -> ContT () m a) -> Delegate r m a
delegate' = coerce

delegate'' :: (r -> (a -> m ()) -> m ()) -> Delegate r m a
delegate'' = delegate' . coerce

runDelegate' :: Delegate r m a -> r -> ContT () m a
runDelegate' = coerce

runDelegate'' :: Delegate r m a -> r -> (a -> m ()) -> m ()
runDelegate'' d r = runContT (runDelegate' d r)

-- -- Activate left after the right, firing results from both activators.
-- -- The binary associative function for 'nulInitializer'.
-- instance
--     ( Applicative m
--     , ChooseBoth c1 c2 c3
--     ) => Z.Also (Delegate r m) (Which c1) (Which c2) (Which c3) where
--     x `also` y = (diversify <$> x) <> (diversify <$> y)
