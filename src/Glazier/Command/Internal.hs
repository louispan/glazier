{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.Command.Internal where

import qualified Data.DList as DL
import Data.IORef

-- This is not an instance of MonadIO or MonadTrans as we don't want to have arbitrary IO effeccts.
newtype NonBlocking m a = NonBlocking (m a)
    deriving (Functor, Applicative, Monad)

unNonBlocking :: NonBlocking m a -> m a
unNonBlocking (NonBlocking m) = m

-- | (read all the results from a TQueue, write a value to a TQueue)
newBusIO :: NonBlocking IO (NonBlocking IO [a], a -> NonBlocking IO ())
newBusIO = NonBlocking $
    (\v -> (NonBlocking $ DL.toList <$> readIORef v
    , \a -> NonBlocking $ atomicModifyIORef v (\b -> (b <> DL.singleton a, ())))
    ) <$> (newIORef DL.empty)
