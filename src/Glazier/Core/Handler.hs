-- {-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.Core.Handler where

import Control.Applicative
import Control.Arrow
import Control.Arrow.Esoteric
import Data.Diverse.Profunctor
import Data.Function
import qualified Glazier.Core.Also as Z
import qualified Glazier.Core.Delegate as Z
import qualified Glazier.Core.Obj as Z

-- | Handle a input @a@ and fire a event @b@
type Handler s m a b = a -> Z.Delegate s m b
type ObjHandler v s m a b = Handler (Z.Obj v s) m a b

-- obviousHandler :: Handler s m a b -> Handler s m (Which '[a]) b
-- obviousHandler hdl = hdl . obvious

-- contramapHandler :: (a1 -> a2) -> Handler s m a2 b -> Handler s m a1 b
-- contramapHandler f hdl = hdl . f

-- mapHandler :: (b1 -> b2) -> Handler s m a b1 -> Handler s m a b2
-- mapHandler = fmap . fmap

-- memptyHandler :: Applicative m => Handler s m a b
-- memptyHandler = mempty

-- mappendHandler :: Applicative m => Handler s m a b -> Handler s m a b -> Handler s m a b
-- mappendHandler = mappend
-- infixr 6 `mappendHandler` -- like mappend

-- ignoreHandler :: forall a m s. Applicative m => Handler s m a ()
-- ignoreHandler = (const @_ @a) mempty

-- arrowHandler :: (a -> b) -> Handler s m a b
-- arrowHandler f = rk $ arr f

-- Chain the output from one handler into the input of the other.
intoH ::
    Handler s m a b
    -> Handler s m b c
    -> Handler s m a c
intoH f g = f & rk2 (>>>) $ g

-- wack :: Handler s m a b -> Handler s m a b
-- wack f = f & E.underKleisli2 (>>>) $ (arrowHandler id) & E.underKleisli2 (>>>) $ (arrowHandler id)

-- Chain the output from one handler into the input of the other.
intoH' :: (Injected a2 b1 b2 b3)
    => Handler s m a (Which b1)
    -> Handler s m (Which a2) (Which b2)
    -> Handler s m a (Which b3)
intoH' f g = f & rk2 (>||>) $ g

-- | Run one or the other.
-- Compile error if types in @a1@ are not distinct from types in @a2@
-- A binary associative function for 'nulHandler'.
orH :: ChooseBetween a1 a2 a3 b1 b2 b3
    => Handler s m (Which a1) (Which b1)
    -> Handler s m (Which a2) (Which b2)
    -> Handler s m (Which a3) (Which b3)
orH f g = f & rk2 (+||+) $ g

thenH :: Handler s m a () -> Handler s m a b -> Handler s m a b
thenH = liftA2 (*>)

-- Run left and also the right, and combine the output type
-- A binary associative function for 'nulHandler'.
alsoH :: (Applicative m, ChooseBoth b1 b2 b3)
    => Handler s m a (Which b1)
    -> Handler s m a (Which b2)
    -> Handler s m a (Which b3)
alsoH = liftA2 Z.also
infixr 6 `alsoH` -- like mappend

maybeH :: Applicative m
    => Handler s m a b
    -> Handler s m (Maybe a) b
maybeH hdl ma = case ma of
    Nothing -> mempty
    Just a -> hdl a

-- arr :: (a -> b) -> w a b
-- first :: w a b -> w (b, d) (c, d)
-- *** :: w b c -> w b' c' -> w (b, b') (c, c')
