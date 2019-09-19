{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.Logger where

import Control.Monad.Context
import qualified Data.List as L
import Data.Maybe
import Data.String
import Data.Tagged.Extras
import GHC.Stack
import Glazier.Command
import Glazier.ShowIO
import GHC.Stack.Extras

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

-- Based on 'GHC.Stack.prettyCallstack'
prettyCallStack' :: (Semigroup str, IsString str) => str -> [(String, SrcLoc)] -> Maybe str
prettyCallStack' delim cs = case cs of
    [] -> Nothing
    xs -> Just . foldr (<>) "" . L.intersperse delim $ prettyCallSite' <$> xs

trimmedCallstack :: Maybe LogCallStackDepth -> [(String, SrcLoc)] -> [(String, SrcLoc)]
trimmedCallstack depth cs = maybe cs ((`take` cs) . untag' @"LogCallStackDepth") depth

prettyCallSite' :: (Semigroup str, IsString str) => (String, SrcLoc) -> str
prettyCallSite' (f, loc) = fromString f <> "@" <> prettySrcLoc' loc

prettySrcLoc' :: (Semigroup str, IsString str) => SrcLoc -> str
prettySrcLoc' SrcLoc {..}
    = foldr (<>) "" $ L.intersperse ":"
        [ fromString srcLocModule
        , fromString $ show srcLocStartLine
        , fromString $ show srcLocStartCol
        ]

--------------------------------------------------------------------

-- | Nothing means do not log, else it has the allowed log level
type AskLogLevel = MonadAsk (Maybe LogLevel)
askLogLevel :: AskLogLevel m => m (Maybe LogLevel)
askLogLevel = askContext

type LogCallStackDepth = Tagged "LogCallStackDepth" Int
-- | Nothing means don't change default callstack depth,
-- Just Nothing means full callstack
-- else limit by the depth
type AskLogCallStackDepth = MonadAsk (Maybe (Maybe LogCallStackDepth))
askLogCallStackDepth :: AskLogCallStackDepth m => m (Maybe (Maybe LogCallStackDepth))
askLogCallStackDepth = askContext

--------------------------------------------------------------------

data LogLevel
    = TRACE -- ^ also print full callstack
    | DEBUG -- ^ also print top callstack
    | INFO_ -- ^ underscore to make the log levels the same character width
    | WARN_ -- ^ also print top callstack, underscore to make the log levels the same character width
    | ERROR -- ^ will also print callstack
    deriving (Eq, Show, Read, Ord)

defaultLogCallStackDepth :: LogLevel -> Maybe LogCallStackDepth
defaultLogCallStackDepth TRACE = Nothing
defaultLogCallStackDepth DEBUG = Just (Tagged @"LogCallStackDepth" 1)
defaultLogCallStackDepth INFO_ = Just (Tagged @"LogCallStackDepth" 0)
defaultLogCallStackDepth WARN_ = Just (Tagged @"LogCallStackDepth" 1)
defaultLogCallStackDepth ERROR = Nothing

-- | allowedLevel logLevel Id msg callstack
data LogLine str = LogLine [(String, SrcLoc)] LogLevel (IO str)

instance (Semigroup str, IsString str) => ShowIO str (LogLine str) where
    showsPrecIO p (LogLine cs lvl msg) = showParenIO (p >= 11) $
        (\msg' ->
            (showStr "LogLine ")
            . (showFromStr $ show lvl)
            . showStr msg'
            . maybe (showStr "") (showStr . (" at " <>)) (prettyCallStack' "; " cs)
        ) <$> msg
type Logger str c m = (Cmd (LogLine str) c, MonadCommand c m, AskLogCallStackDepth m, AskLogLevel m)

-- logged :: (HasCallStack => Monad m, ShowIO str a) => (HasCallStack => LogLevel -> IO str -> m ()) -> (a -> m b) -> LogLevel -> a -> m b
-- logged logLn go lvl a = withoutCallStack $ do
--     logLn lvl (showIO a)
--     go a

logLine :: (HasCallStack, Logger str c m)
    => (LogLevel -> Maybe LogCallStackDepth) -> LogLevel -> IO str
    -> m ()
logLine f lvl msg = withoutCallStack $ do
    allowedLevel <- askLogLevel
    case allowedLevel of
        Nothing -> pure ()
        Just allowedLvl'
            | lvl >= allowedLvl' -> do
                d <- askLogCallStackDepth
                let d' = fromMaybe (f lvl) d
                    cs' = trimmedCallstack d' $ getCallStack callStack
                exec $ LogLine cs' lvl msg
            | otherwise -> pure ()
