{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.Command.Internal where

newtype NewEmptyMVar a = NewEmptyMVar (IO a)
    deriving (Functor, Applicative, Monad)

unNewEmptyMVar :: NewEmptyMVar a -> IO a
unNewEmptyMVar (NewEmptyMVar m) = m
