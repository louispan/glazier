{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.Command.Internal where

import Control.Concurrent.Chan

newtype NewChanIO a = NewChanIO (IO a)
    deriving (Functor, Applicative, Monad)

unNewChanIO :: NewChanIO a -> IO a
unNewChanIO (NewChanIO m) = m

newChanIO :: NewChanIO (Chan a)
newChanIO = NewChanIO newChan
