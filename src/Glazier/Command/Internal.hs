{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.Command.Internal where

import Control.Concurrent.STM

-- This is not an instance of MonadIO or MonadTrans as we don't want to have arbitrary IO effeccts.
newtype NonBlocking m a = NonBlocking (m a)
    deriving (Functor, Applicative, Monad)

-- The Cargo is something than can only be read from once
-- The read is blocks until it is marked ready to be read by another thread.
-- The sending thread never blocks, but you can't send (does nothing)
-- after the bus is consumed.
-- data Cargo a = CargoConsumed | CargoReady (Chan a) | CargoConsolidating (Chan a)

unNonBlocking :: NonBlocking m a -> m a
unNonBlocking (NonBlocking m) = m

newBusIO :: NonBlocking IO (NonBlocking IO [a], a -> NonBlocking IO ())
newBusIO = NonBlocking $
    -- smaller atomic stm transactions have better performance
    -- https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch10.html#sec_stm-cost
    (\v -> (NonBlocking $ atomically $ flushTQueue v
    , NonBlocking . atomically . writeTQueue v)) <$> newTQueueIO
