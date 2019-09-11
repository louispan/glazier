{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.Logger where

import Control.Monad.Context
import Control.Monad.IO.Class
import qualified Data.List as DL
import Data.Proxy
import Data.String
import Data.Tagged.Extras
import GHC.Stack
import Glazier.Command
import Glazier.ShowIO


#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

wack :: LogCallstackDepth -> Int
wack = untag' @"LogCallstackDepth"

-- Based on 'GHC.Stack.prettyCallstack'
prettyTrimmedCallStack' :: (Semigroup str, IsString str) => str -> Maybe LogCallstackDepth -> CallStack -> Maybe str
prettyTrimmedCallStack' delim depth cs = case trimmedCs of
    [] -> Nothing
    xs -> Just . foldr (<>) "" . DL.intersperse delim $ prettyCallSite' <$> xs
  where
    cs' = getCallStack cs
    trimmedCs = maybe cs' ((`take` cs') . untag' @"LogCallstackDepth") depth

prettyCallSite' :: (Semigroup str, IsString str) => (String, SrcLoc) -> str
prettyCallSite' (f, loc) = fromString f <> "@" <> prettySrcLoc' loc

prettySrcLoc' :: (Semigroup str, IsString str) => SrcLoc -> str
prettySrcLoc' SrcLoc {..}
    = foldr (<>) "" $ DL.intersperse ":"
        [ fromString srcLocModule
        , fromString $ show srcLocStartLine
        , fromString $ show srcLocStartCol
        ]

--------------------------------------------------------------------

-- | Nothing means do not log, else it has the allowed log level
type AskLogLevel = MonadAsk (IO (Maybe LogLevel))
askLogLevel :: AskLogLevel m => m (IO (Maybe LogLevel))
askLogLevel = askContext

type LogCallstackDepth = Tagged "LogCallstackDepth" Int
-- | Nothing means don't change default callstack depth,
-- Just Nothing means full callstack
-- else limit by the depth
type AskLogCallstackDepth = MonadAsk (IO (Maybe (Maybe LogCallstackDepth)))
askLogCallstackDepth :: AskLogCallstackDepth m => m (IO (Maybe (Maybe LogCallstackDepth)))
askLogCallstackDepth = askContext

type LogName str = Tagged "LogName" str
type AskLogName str = MonadAsk (LogName str)
askLogName :: AskLogName str m => m (LogName str)
askLogName = askContext

--------------------------------------------------------------------

data LogLevel
    = TRACE -- ^ also print full callstack
    | DEBUG -- ^ also print top callstack
    | INFO_ -- ^ underscore to make the log levels the same character width
    | WARN_ -- ^ also print top callstack, underscore to make the log levels the same character width
    | ERROR -- ^ will also print callstack
    deriving (Eq, Show, Read, Ord)

defaultLogCallstackDepth :: LogLevel -> Maybe LogCallstackDepth
defaultLogCallstackDepth TRACE = Nothing
defaultLogCallstackDepth DEBUG = Just (Tagged @"LogCallstackDepth" 1)
defaultLogCallstackDepth INFO_ = Just (Tagged @"LogCallstackDepth" 0)
defaultLogCallstackDepth WARN_ = Just (Tagged @"LogCallstackDepth" 1)
defaultLogCallstackDepth ERROR = Nothing

-- | allowedLevel callstackDepthOverride logLevel logname callstack msg
data LogLine str = LogLine LogLevel (Maybe LogCallstackDepth) (LogName str) CallStack (IO str)

instance (Semigroup str, IsString str) => ShowIO str (LogLine str) where
    showsPrecIO p (LogLine lvl d n cs msg) = showParenIO (p >= 11) $
        (\msg' ->
            (showStr "LogLine ")
            . (showFromStr $ show lvl)
            . maybe (showStr "*") (showFromStr . show . untag') d
            . showStr " " . showStr (untag n) . showStr " " . showStr msg'
            . maybe (showStr "") (showStr . (" at " <>)) (prettyTrimmedCallStack' "; " d cs)
        ) <$> msg
type Logger str c m = (Cmd (LogLine str) c, MonadCommand c m, MonadIO m, AskLogCallstackDepth m, AskLogLevel m, AskLogName str m)

logLine :: (Logger str c m)
    => Proxy str -> LogLevel -> CallStack -> IO str
    -> m ()
logLine _ lvl cs msg = do
    allowedLevel <- askLogLevel >>= liftIO
    case allowedLevel of
        Nothing -> pure ()
        Just allowedLvl'
            | lvl >= allowedLvl' -> do
                n <- askLogName
                d <- askLogCallstackDepth >>= liftIO
                let d' = case d of
                        Nothing -> defaultLogCallstackDepth lvl
                        Just d'' -> d''
                exec $ LogLine lvl d' n cs msg
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
