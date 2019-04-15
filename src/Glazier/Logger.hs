{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.Logger where

import Control.Lens
import Control.Monad.Reader
import Data.Diverse.Lens
import Data.Maybe
import Data.Semigroup
import qualified Data.Text.Lazy as TL
import GHC.Stack
import Glazier.Benign
import Glazier.Command

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

type Logger r c m = (AsFacet LogLine c, MonadCommand c m, MonadReader r m, Has (Benign IO (Maybe LogLevel)) r)

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

_logLevel :: Has (Benign IO (Maybe LogLevel)) s => Lens' s (Benign IO (Maybe LogLevel))
_logLevel = hasLens @(Benign IO (Maybe LogLevel))

logLine :: (Logger r c m)
    => LogLevel -> CallStack -> Benign IO TL.Text
    -> m ()
logLine lvl cs m = do
    lvl' <- view _logLevel
    exec $ LogLine lvl' lvl cs m

logExec :: (Show cmd, AsFacet cmd c, Logger r c m)
    => LogLevel -> CallStack -> cmd -> m ()
logExec lvl cs c = do
    logLine lvl cs (pure . TL.pack $ show c)
    exec c

logExec' :: (Show (cmd c), AsFacet (cmd c) c, Logger r c m)
    => LogLevel -> CallStack -> cmd c -> m ()
logExec' lvl cs c = do
    logLine lvl cs (pure . TL.pack $ show c)
    exec' c

logEval :: (Show cmd, AsFacet cmd c, Logger r c m)
    => LogLevel -> CallStack -> ((a -> c) -> cmd) -> m a
logEval lvl cs k = delegatify $ logExec lvl cs . k

logEval' :: (Show (cmd c), AsFacet (cmd c) c, Logger r c m)
    => LogLevel -> CallStack -> ((a -> c) -> cmd c) -> m a
logEval' lvl cs k = delegatify $ logExec' lvl cs . k

logInvoke :: (Show (cmd c), AsFacet (cmd c) c, Functor cmd, Logger r c m)
    => LogLevel -> CallStack -> cmd a -> m a
logInvoke lvl cs c = logEval' lvl cs (<$> c)
