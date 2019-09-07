{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.Logger where

import Control.Monad.Context
import Data.Diverse.Lens
import Data.Maybe
import qualified Data.Text as T
import Glazier.Command
import Glazier.ShowIO
import GHC.Stack


#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

-- Modified from GHC.Stack to be shorter, to use T.Text
-- and to operate on [(String, SrcLoc)]
prettyCallStack' :: CallStack -> T.Text
prettyCallStack' = T.intercalate "\n " . fmap prettyCallSite' . getCallStack
  where
    prettyCallSite' :: (String, SrcLoc) -> T.Text
    prettyCallSite' (f, loc) = T.pack f <> "@" <> prettySrcLoc' loc

    prettySrcLoc' :: SrcLoc -> T.Text
    prettySrcLoc' SrcLoc {..}
        = foldr (<>) ""
            [ T.pack srcLocModule
            , ":"
            , T.pack $ show srcLocStartLine
            , ":"
            , T.pack $ show srcLocStartCol
            ]

callStackTop :: CallStack -> Maybe (String, SrcLoc)
callStackTop = listToMaybe . getCallStack

--------------------------------------------------------------------

type AskLogLevel = MonadAsk (IO (Maybe LogLevel))
askLogLevel :: AskLogLevel m => m (IO (Maybe LogLevel))
askLogLevel = askContext

--------------------------------------------------------------------

-- type Logger c m = (AsFacet LogLine c, MonadCommand c m, AskLogLevel m)

data LogLevel
    = TRACE
    | DEBUG
    | INFO_ -- ^ underscore to make the log levels the same character width
    | WARN_ -- ^ underscore to make the log levels the same character width
    | ERROR -- ^ will also print callstack
    deriving (Eq, Show, Read, Ord)

data LogLine = LogLine (IO (Maybe LogLevel)) LogLevel CallStack (IO T.Text)

instance Show LogLine where
    showsPrec p (LogLine _ lvl cs _) = showParen (p >= 11) $
        showString "LogLine " . shows lvl . showString " at " . showString (T.unpack $ prettyCallStack' cs)

logLine :: (AsFacet LogLine c, MonadProgram c m, AskLogLevel m)
    => LogLevel -> CallStack -> IO T.Text
    -> m ()
logLine lvl cs m = do
    lvl' <- askLogLevel
    exec $ LogLine lvl' lvl cs m

logExec :: (ShowIO cmd, AsFacet cmd c, AsFacet LogLine c, MonadProgram c m, AskLogLevel m)
    => LogLevel -> CallStack -> cmd -> m ()
logExec lvl cs c = do
    logLine lvl cs $ showIO c
    exec c

logExec' :: (ShowIO (cmd c), AsFacet (cmd c) c, AsFacet LogLine c, MonadProgram c m, AskLogLevel m)
    => LogLevel -> CallStack -> cmd c -> m ()
logExec' lvl cs c = do
    logLine lvl cs $ showIO c
    exec' c

logEval :: (ShowIO cmd, AsFacet cmd c, AsFacet LogLine c, MonadCommand c m, AskLogLevel m)
    => LogLevel -> CallStack -> ((a -> c) -> cmd) -> m a
logEval lvl cs k = delegatify $ logExec lvl cs . k

logEval' :: (ShowIO (cmd c), AsFacet (cmd c) c, AsFacet LogLine c, MonadCommand c m, AskLogLevel m)
    => LogLevel -> CallStack -> ((a -> c) -> cmd c) -> m a
logEval' lvl cs k = delegatify $ logExec' lvl cs . k

logInvoke :: (ShowIO (cmd c), AsFacet (cmd c) c, Functor cmd, AsFacet LogLine c, MonadCommand c m, AskLogLevel m)
    => LogLevel -> CallStack -> cmd a -> m a
logInvoke lvl cs c = logEval' lvl cs (<$> c)

logInvoke_ :: (ShowIO (cmd c), AsFacet (cmd c) c, AsFacet [c] c, Functor cmd, AsFacet LogLine c, MonadProgram c m, AskLogLevel m)
    => LogLevel -> CallStack -> cmd () -> m ()
logInvoke_ lvl cs = logExec' lvl cs . fmap command_
