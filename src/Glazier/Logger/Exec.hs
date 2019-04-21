{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Glazier.Logger.Exec where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Data.Text.Lazy as TL
import Data.Time
import GHC.Stack
import Glazier.Benign.Internal
import Glazier.Logger

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

execLogger :: LogLine
    -> Benign IO (Maybe (LogLevel, CallStack, TL.Text))
execLogger (LogLine lvl' lvl cs entry) = runMaybeT $ do
    allowedLvl <- MaybeT lvl'
    guard (allowedLvl >= lvl)
    lift $ (\a -> (lvl, cs, a)) <$> entry

defaultLogLine :: LogLevel -> CallStack -> TL.Text -> Benign IO TL.Text
defaultLogLine lvl cs entry = do
    dt <- Benign getCurrentTime
    -- todo get time, etc
    let dt' = TL.pack $ formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S.%q")) dt
    pure $ dt' <> " " <> (TL.pack $ show lvl) <> " " <> entry <> prettyErrorStack lvl
  where
    -- Return a stack only if ERROR, otherwise mempty
    prettyErrorStack ERROR = " [" <> prettyCallStack' cs <> "]"
    prettyErrorStack _ = mempty
