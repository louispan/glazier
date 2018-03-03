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
import Data.Diverse.Profunctor
import qualified Glazier.Core.Also as Z
import qualified Glazier.Core.Delegate as Z
import qualified Glazier.Core.Obj as Z

-- | Handle a input @a@ and fire a event @b@
type Handler s m a b = a -> Z.Delegate s m b
type ObjHandler v s m a b = Handler (Z.Obj v s) m a b


obviousHandler :: Handler s m a b -> Handler s m (Which '[a]) b
obviousHandler hdl = hdl . obvious

contramapHandler :: (a1 -> a2) -> Handler s m a2 b -> Handler s m a1 b
contramapHandler f hdl = hdl . f

mapHandler :: (b1 -> b2) -> Handler s m a b1 -> Handler s m a b2
mapHandler = fmap . fmap

memptyHandler :: Applicative m => Handler s m a b
memptyHandler = mempty

mappendHandler :: Applicative m => Handler s m a b -> Handler s m a b -> Handler s m a b
mappendHandler = mappend
infixr 6 `mappendHandler` -- like mappend

ignoreHandler :: forall a m s. Applicative m => Handler s m a ()
ignoreHandler = (const @_ @a) mempty

-- noopHandler :: forall a m s. Applicative m => Handler m s a ()
-- noopHandler = memptyHandler @a @()

-- | Identify for 'orHandler' or 'alsoHandler'
-- nulHandler :: Applicative m => Handler m r (Which '[]) (Which '[])
-- nulHandler = memptyHandler @(Which '[]) @(Which '[])

arrowHandler :: (a -> b) -> Handler s m a b
arrowHandler f = runKleisli $ arr f

-- Chain the output from one handler into the input of the other.
intoHandler ::
    Handler s m a b
    -> Handler s m b c
    -> Handler s m a c
intoHandler f g = runKleisli $ Kleisli f >>> Kleisli g

-- Chain the output from one handler into the input of the other.
intoHandler' :: (Injected a2 b1 b2 b3)
    => Handler s m a (Which b1)
    -> Handler s m (Which a2) (Which b2)
    -> Handler s m a (Which b3)
intoHandler' f g = runKleisli $ Kleisli f >||> Kleisli g

-- | Run one or the other.
-- Compile error if types in @a1@ are not distinct from types in @a2@
-- A binary associative function for 'nulHandler'.
orHandler :: ChooseBetween a1 a2 a3 b1 b2 b3
    => Handler s m (Which a1) (Which b1)
    -> Handler s m (Which a2) (Which b2)
    -> Handler s m (Which a3) (Which b3)
orHandler f g = runKleisli $ Kleisli f +||+ Kleisli g

thenHandler :: Handler s m a () -> Handler s m a b -> Handler s m a b
thenHandler = liftA2 (*>)

-- Run left after the right, and combine the output type
-- A binary associative function for 'nulHandler'.
alsoHandler :: (Applicative m, ChooseBoth b1 b2 b3)
    => Handler s m a (Which b1)
    -> Handler s m a (Which b2)
    -> Handler s m a (Which b3)
alsoHandler = liftA2 Z.also
infixr 6 `alsoHandler` -- like mappend

maybeHandle :: Applicative m
    => Handler s m a b
    -> Handler s m (Maybe a) b
maybeHandle hdl ma = case ma of
    Nothing -> mempty
    Just a -> hdl a

-- -- | A friendlier constraint synonym for 'pretend'.
-- type Pretend a2 a3 b2 b3 =
--     ( Reinterpret a2 a3
--     , b3 ~ AppendUnique (Complement a3 a2) b2 -- ^ redundant constraint: but limits the output type
--     , Diversify b2 b3
--     , Diversify (Complement a3 a2) b3
--     )

-- -- | Change the types the handler can handle by passing any extra
-- -- input directly to the output.
-- -- @a2@ contains the input type to convert into
-- -- AllowAmbiguousTypes@: Use @TypeApplications@ instead of @Proxy@ to specify @a3@
-- pretend :: forall a3 m s a2 b2 b3.
--     (Pretend a2 a3 b2 b3)
--     => Handler s m (Which a2) (Which b2)
--     -> Handler s m (Which a3) (Which b3)
-- pretend hdl a = case reinterpret a of
--         Left a' -> pure $ diversify a'
--         Right a' -> diversify <$> hdl a'
