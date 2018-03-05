{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.Core.Meld where

import Control.Applicative
import Data.Biapplicative
import Data.Diverse.Profunctor
import Data.Semigroup

also :: (Semigroup (f (Which a3)), Functor f, ChooseBoth a1 a2 a3) => f (Which a1) -> f (Which a2) -> f (Which a3)
also x y = (diversify <$> x) <> (diversify <$> y)
infixr 6 `also` -- like mappend

alternatively :: (Alternative f, ChooseBoth a1 a2 a3) => f (Which a1) -> f (Which a2) -> f (Which a3)
alternatively x y = (diversify <$> x) <|> (diversify <$> y)
infixl 3 `alternatively` -- like <|>

type Besides a1 a2 a3 = (a3 ~ Append a1 a2)

besides :: (Applicative f, Besides a1 a2 a3)
    => f (Many a1) -> f (Many a2) -> f (Many a3)
besides x y = liftA2 (/./) x y
infixr 5 `besides` -- like (/./) and (++)

-- besides' :: (Applicative f)
--     => f () -> f a2 -> f a2
-- besides' x y = liftA2 (*>) x y
-- infixr 5 `besides` -- like (/./) and (++)

besides2 :: (Biapplicative f, Besides a1 a2 a3, Besides b1 b2 b3)
    => f (Many a1) (Many b1) -> f (Many a2) (Many b2) -> f (Many a3) (Many b3)
besides2 x y = biliftA2 (/./) (/./) x y
infixr 5 `besides2` -- like (/./) and (++)
