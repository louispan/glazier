{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Glazier.Core.Also where

class Also f a b c | a b -> c where
    also :: f a -> f b -> f c
infixr 6 `also` -- like mappend
