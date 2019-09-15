{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.Logger where

import Control.Monad.Context
import qualified Data.List as L
import Data.Maybe
import Data.Proxy
import Data.String
import Data.Tagged.Extras
import GHC.Stack
import Glazier.Command
import Glazier.ShowIO

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

type AskLogName str = MonadAsk (Tagged "LogName" str)
askLogName :: AskLogName str m => m (Tagged "LogName" str)
askLogName = askContext

--------------------------------------------------------------------

data LogLevel
    = TRACE -- ^ also print full callstack
    | DEBUG -- ^ also print top callstack
    | INFO_ -- ^ underscore to make the log levels the same character width
    | WARN_ -- ^ also print top callstack, underscore to make the log levels the same character width
    | ERROR -- ^ will also print callstack
    deriving (Eq, Show, Read, Ord)

logLevelCallStackDepth :: LogLevel -> Maybe LogCallStackDepth
logLevelCallStackDepth TRACE = Nothing
logLevelCallStackDepth DEBUG = Just (Tagged @"LogCallStackDepth" 1)
logLevelCallStackDepth INFO_ = Just (Tagged @"LogCallStackDepth" 0)
logLevelCallStackDepth WARN_ = Just (Tagged @"LogCallStackDepth" 1)
logLevelCallStackDepth ERROR = Nothing

-- | allowedLevel logLevel logname LogId msg callstack
data LogLine str = LogLine LogLevel (Tagged "LogName" str) (IO str) [(String, SrcLoc)]

instance (Semigroup str, IsString str) => ShowIO str (LogLine str) where
    showsPrecIO p (LogLine lvl n msg cs) = showParenIO (p >= 11) $
        (\msg' ->
            (showStr "LogLine ")
            . (showFromStr $ show lvl)
            . showStr " " . showStr (untag' @"LogName" n) . showStr " "
            . showStr msg'
            . maybe (showStr "") (showStr . (" at " <>)) (prettyCallStack' "; " cs)
        ) <$> msg
type Logger str c m = (Cmd (LogLine str) c, MonadCommand c m, AskLogCallStackDepth m, AskLogLevel m, AskLogName str m)

logLine :: (Logger str c m)
    => Proxy str -> LogLevel -> CallStack -> IO str
    -> m ()
logLine _ lvl cs msg = do
    allowedLevel <- askLogLevel
    case allowedLevel of
        Nothing -> pure ()
        Just allowedLvl'
            | lvl >= allowedLvl' -> do
                n <- askLogName
                d <- askLogCallStackDepth
                let d' = fromMaybe (logLevelCallStackDepth lvl) d
                    cs' = trimmedCallstack d' $ getCallStack cs
                exec $ LogLine lvl n msg cs'
            | otherwise -> pure ()

logExec :: (ShowIO str cmd, Cmd cmd c, Logger str c m)
    => Proxy str -> LogLevel -> CallStack -> cmd -> m ()
logExec p lvl cs c = do
    logLine p lvl cs $ showIO c
    exec c

logExec' :: (ShowIO str (cmd c), Cmd' cmd c, Logger str c m)
    => Proxy str -> LogLevel -> CallStack -> cmd c -> m ()
logExec' p lvl cs c = do
    logLine p lvl cs $ showIO c
    exec' c

logEval :: (ShowIO str cmd, Cmd cmd c, Logger str c m)
    => Proxy str -> LogLevel -> CallStack -> ((a -> c) -> cmd) -> m a
logEval p lvl cs k = delegatify $ logExec p lvl cs . k

logEval' :: (ShowIO str (cmd c), Cmd' cmd c, Logger str c m)
    => Proxy str -> LogLevel -> CallStack -> ((a -> c) -> cmd c) -> m a
logEval' p lvl cs k = delegatify $ logExec' p lvl cs . k

logInvoke :: (ShowIO str (cmd c), Cmd' cmd c, Functor cmd, Logger str c m)
    => Proxy str -> LogLevel -> CallStack -> cmd a -> m a
logInvoke p lvl cs c = logEval' p lvl cs (<$> c)

logInvoke_ :: (ShowIO str (cmd c), Cmd' cmd c, Cmd' [] c, Functor cmd, Logger str c m)
    => Proxy str -> LogLevel -> CallStack -> cmd () -> m ()
logInvoke_ p lvl cs = logExec' p lvl cs . fmap command_
