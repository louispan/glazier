{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.Command.Internal where

import qualified Pipes.Concurrent as PC

newtype NewBusIO a = NewBusIO (IO a)
    deriving (Functor, Applicative, Monad)

unNewBusIO :: NewBusIO a -> IO a
unNewBusIO (NewBusIO m) = m

newBusIO :: NewBusIO (PC.Output a, PC.Input a)
newBusIO = NewBusIO $ PC.spawn PC.unbounded
