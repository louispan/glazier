{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.Command.Internal where

import qualified Data.DList as DL
import Data.IORef

-- | (read all the results from a mutable list, write a value to a mutable list)
newBusIO :: IO (IO [a], a -> IO ())
newBusIO =
    (\v -> (DL.toList <$> readIORef v
    , \a -> atomicModifyIORef v (\b -> (b <> DL.singleton a, ())))
    ) <$> (newIORef DL.empty)
