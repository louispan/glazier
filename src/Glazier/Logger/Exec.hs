{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Glazier.Logger.Exec where

import qualified Data.Text as T
import Data.Time
import GHC.Stack
import Control.Monad.Benign
import Control.Monad.Benign.Internal
import Glazier.Logger

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

-- basicLogLine :: MonadIO m => LogLine -> m (UTCTime, LogLevel, CallStack, T.Text)
-- basicLogLine (LogLine lvl cs entry) = (\dt a -> (dt, lvl, cs, a))
--     <$> (liftIO $ getCurrentTime)
--     <*> (liftIO entry)

-- simLogLine :: MonadIO m => LogLine -> m (UTCTime, LogLevel, CallStack, T.Text)
-- simLogLine (LogLine lvl cs entry) = (\dt a -> (dt, lvl, cs, a))
--     <$> (liftIO $ getCurrentTime)
--     <*> (liftIO entry)
