{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.Core.Handler where

import Control.Arrow
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Cont.Extras as TE
import Data.Diverse.Profunctor
import qualified Glazier.Core.Obj as Z

-- | Handle a input @a@ and fire a event @b@
type Handler m s a b = s -> a -> ContT () m b
type ObjHandler m v s a b = Handler m (Z.Obj v s) a b

-- -- | Identify for 'orHandler'' or 'alsoHandler''
-- nulHandler' :: Applicative m => Handler m r (Which '[]) ()
-- nulHandler' _ _ = ContT $ \_ -> pure ()

-- -- Run left after the right.
-- -- A binary associative function for 'nulHandler''.
-- alsoHandler' :: (Applicative m)
--     => Handler m s a ()
--     -> Handler m s a ()
--     -> Handler m s a ()
-- alsoHandler' f g s a = (f s a) `TE.alsoContT` (g s a)
-- infixr 6 `alsoHandler'` -- like mappend

-- -- | Run one or the other.
-- -- Compile error if types in @a1@ are not distinct from types in @a2@
-- -- A binary associative function for 'nulHandler''.
-- orHandler' :: forall m s a1 a2 a3. ChooseFrom a1 a2 a3
--     => Handler m s (Which a1) ()
--     -> Handler m s (Which a2) ()
--     -> Handler m s (Which a3) ()
-- orHandler' f g s a = case reinterpret @a2 @a3 a of
--     Left a1 -> f s a1
--     Right a2 -> g s a2
-- infixr 6 `orHandler'` -- like mappend
arrowHandler :: (a -> b) -> Handler m s a b
arrowHandler f _ =  runKleisli $  arr f

obviousHandler :: Handler m s a b -> Handler m s (Which '[a]) b
obviousHandler = contramapHandler obvious

contramapHandler :: (a1 -> a2) -> Handler m s a2 b -> Handler m s a1 b
contramapHandler f hdl s a = hdl s (f a)

mapHandler :: (b1 -> b2) -> Handler m s a b1 -> Handler m s a b2
mapHandler f hdl s a = f <$> hdl s a

memptyHandler :: forall a b m s. Applicative m => Handler m s a b
memptyHandler _ _ = ContT $ \_ -> pure ()

mappendHandler :: Applicative m => Handler m s a b -> Handler m s a b -> Handler m s a b
mappendHandler f g s a = f s a `TE.alsoContT` g s a
infixr 6 `mappendHandler` -- like mappend

-- ignoreHandler :: forall a m s. Applicative m => Handler m s a (Which '[])
-- ignoreHandler = memptyHandler @a @(Which '[])

noopHandler :: forall a m s. Applicative m => Handler m s a ()
noopHandler = memptyHandler @a @()

-- | Identify for 'orHandler' or 'alsoHandler'
nulHandler :: Applicative m => Handler m r (Which '[]) (Which '[])
nulHandler = memptyHandler @(Which '[]) @(Which '[])

-- | Run one or the other.
-- Compile error if types in @a1@ are not distinct from types in @a2@
-- A binary associative function for 'nulHandler'.
orHandler :: ChooseBetween a1 a2 a3 b1 b2 b3
    => Handler m s (Which a1) (Which b1)
    -> Handler m s (Which a2) (Which b2)
    -> Handler m s (Which a3) (Which b3)
orHandler f g s = runKleisli $ Kleisli (f s) +||+ Kleisli (g s)

-- Run left after the right, and combine the output type
-- A binary associative function for 'nulHandler'.
alsoHandler :: (Applicative m, ChooseBoth b1 b2 b3)
    => Handler m s a (Which b1)
    -> Handler m s a (Which b2)
    -> Handler m s a (Which b3)
alsoHandler f g s a = (diversify <$> f s a) `TE.alsoContT` (diversify <$> g s a)
infixr 6 `alsoHandler` -- like mappend

thenHandler :: Handler m s a () -> Handler m s a b -> Handler m s a b
thenHandler x y s a = x s a *> y s a

-- Chain the output from one handler into the input of the other.
intoHandler :: Applicative m
    => Handler m s a b
    -> Handler m s b c
    -> Handler m s a c
intoHandler f g s a = f s a >>= g s

-- Chain the output from one handler into the input of the other.
intoHandler' :: (Pretend a2 b1 b2 b3, Applicative m)
    => Handler m s a (Which b1)
    -> Handler m s (Which a2) (Which b2)
    -> Handler m s a (Which b3)
intoHandler' f g = intoHandler f (pretend g)

maybeHandle :: Applicative m
    => Handler m s a b
    -> Handler m s (Maybe a) b
maybeHandle hdl s ma = case ma of
    Nothing -> ContT $ \_ -> pure ()
    Just a -> hdl s a

-- | A friendlier constraint synonym for 'pretend'.
type Pretend a2 a3 b2 b3 =
    ( Reinterpret a2 a3
    , b3 ~ Append (Complement a3 a2) b2 -- ^ redundant constraint: but limits the output type
    , Diversify b2 b3
    , Diversify (Complement a3 a2) b3
    )

-- | Change the types the handler can handle by passing any extra
-- input directly to the output.
-- @a2@ contains the input type to convert into
-- AllowAmbiguousTypes@: Use @TypeApplications@ instead of @Proxy@ to specify @a3@
pretend :: forall a3 m s a2 b2 b3.
    (Pretend a2 a3 b2 b3)
    => Handler m s (Which a2) (Which b2)
    -> Handler m s (Which a3) (Which b3)
pretend hdl = go <$> hdl
  where
    go k a = case reinterpret a of
        Left a' -> pure $ diversify a'
        Right a' -> diversify <$> k a'
