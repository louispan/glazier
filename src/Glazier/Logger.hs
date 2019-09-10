{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
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
import qualified Data.List as DL
import Data.Maybe
import Data.Proxy
import Data.String
import GHC.Stack
import Glazier.Command
import Glazier.ShowIO


#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

-- Modified from GHC.Stack to be shorter, to use T.Text
-- and to operate on [(String, SrcLoc)]
prettyCallStack' :: (Semigroup str, IsString str) => CallStack -> str
prettyCallStack' = foldr (<>) "" . DL.intersperse "\n " . fmap prettyCallSite' . getCallStack
  where
    prettyCallSite' :: (Semigroup str, IsString str) => (String, SrcLoc) -> str
    prettyCallSite' (f, loc) = fromString f <> "@" <> prettySrcLoc' loc

    prettySrcLoc' :: (Semigroup str, IsString str) => SrcLoc -> str
    prettySrcLoc' SrcLoc {..}
        = foldr (<>) "" $ DL.intersperse ":"
            [ fromString srcLocModule
            , fromString $ show srcLocStartLine
            , fromString $ show srcLocStartCol
            ]

callStackTop :: CallStack -> Maybe (String, SrcLoc)
callStackTop = listToMaybe . getCallStack

--------------------------------------------------------------------

type AskLogLevel = MonadAsk (IO (Maybe LogLevel))
askLogLevel :: AskLogLevel m => m (IO (Maybe LogLevel))
askLogLevel = askContext

newtype LogName str = LogName { getLogName :: str }
type AskLogName str = MonadAsk (LogName str)
askLogName :: AskLogName str m => m str
askLogName = getLogName <$> askContext

--------------------------------------------------------------------

-- type Logger c m = (AsFacet LogLine c, MonadCommand c m, AskLogLevel m)

data LogLevel
    = TRACE
    | DEBUG
    | INFO_ -- ^ underscore to make the log levels the same character width
    | WARN_ -- ^ underscore to make the log levels the same character width
    | ERROR -- ^ will also print callstack
    deriving (Eq, Show, Read, Ord)

-- | allowedLevel logname logLvel callstack msg
data LogLine str = LogLine (IO (Maybe LogLevel)) str LogLevel CallStack (IO str)

instance (Semigroup str, IsString str) => ShowIO str (LogLine str) where
    showsPrecIO p (LogLine allowedLvl n lvl cs msg) = showParenIO (p >= 11) $
        (\allowedLvl' msg' ->
            ("LogLine ") <> (fromString $ show lvl) <> "/" <> (fromString $ show allowedLvl')
            <> " " <> n <> " " <> msg' <> " at " <> (prettyCallStack' cs)) <$> allowedLvl <*> msg

type Logger str c m = (Cmd (LogLine str) c, MonadCommand c m, AskLogLevel m, AskLogName str m)

logLine :: Logger str c m
    => Proxy str -> LogLevel -> CallStack -> IO str
    -> m ()
logLine _ lvl cs msg = do
    lvl' <- askLogLevel
    n <- askLogName
    exec $ LogLine lvl' n lvl cs msg

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
