{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.Logger where

import Data.Maybe
import Data.Diverse.Lens
import Data.Semigroup
import GHC.Stack
import Glazier.Benign
import Glazier.Command
import qualified Data.Text.Lazy as TL

-- Modified from GHC.Stack to be shorter, to use TL.Text
-- and to operate on [(String, SrcLoc)]
prettyCallStack' :: CallStack -> TL.Text
prettyCallStack' = TL.intercalate "\n " . fmap prettyCallSite' . getCallStack
  where
    prettyCallSite' :: (String, SrcLoc) -> TL.Text
    prettyCallSite' (f, loc) = TL.pack f <> "@" <> prettySrcLoc' loc

    prettySrcLoc' :: SrcLoc -> TL.Text
    prettySrcLoc' SrcLoc {..}
        = foldr (<>) ""
            [ TL.pack srcLocModule
            , ":"
            , TL.pack $ show srcLocStartLine
            , ":"
            , TL.pack $ show srcLocStartCol
            ]

callStackTop :: CallStack -> Maybe (String, SrcLoc)
callStackTop = listToMaybe . getCallStack

type Logger c m = (AsFacet LogLine c, MonadLogLevel m)

instance Show LogLine where
    showsPrec p (LogLine _ lvl cs _) = showParen (p >= 11) $
        showString "LogLine " . shows lvl . showString " at " . showString (TL.unpack $ prettyCallStack' cs)

data LogLevel
    = TRACE
    | DEBUG
    | INFO_
    | WARN_
    -- will also print callstack
    | ERROR
    deriving (Eq, Show, Read, Ord)

data LogLine = LogLine (Benign IO (Maybe LogLevel)) LogLevel CallStack (Benign IO TL.Text)

class Monad m => MonadLogLevel m where
    logLevel :: m (Benign IO (Maybe LogLevel))

newtype LoggerT m a = LoggerT { runLoggerT :: Benign IO (Maybe LogLevel) -> m a }
    deriving Functor
    deriving Applicative via (ReaderT (Benign IO (Maybe LogLevel)))

logLine :: (MonadCommand c m, Logger c m)
    => LogLevel -> CallStack -> Benign IO TL.Text
    -> m ()
logLine lvl cs m = do
    lvl' <- logLevel
    exec $ LogLine lvl' lvl cs m

logExec :: (Show cmd, AsFacet cmd c, MonadCommand c m, Logger c m)
    => LogLevel -> CallStack -> cmd -> m ()
logExec lvl cs c = do
    logLine lvl cs (pure . TL.pack $ show c)
    exec c

logExec' :: (Show (cmd c), AsFacet (cmd c) c, MonadCommand c m, Logger c m)
    => LogLevel -> CallStack -> cmd c -> m ()
logExec' lvl cs c = do
    logLine lvl cs (pure . TL.pack $ show c)
    exec' c

logEval :: (Show cmd, AsFacet cmd c, MonadCommand c m, Logger c m)
    => LogLevel -> CallStack -> ((a -> c) -> cmd) -> m a
logEval lvl cs k = delegate $ logExec lvl cs . k

logEval' :: (Show (cmd c), AsFacet (cmd c) c, MonadCommand c m, Logger c m)
    => LogLevel -> CallStack -> ((a -> c) -> cmd c) -> m a
logEval' lvl cs k = delegate $ logExec' lvl cs . k

logInvoke :: (Show (cmd c), AsFacet (cmd c) c, MonadCommand c m, Logger c m, Functor cmd)
    => LogLevel -> CallStack -> cmd a -> m a
logInvoke lvl cs c = logEval' lvl cs (<$> c)
