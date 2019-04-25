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

import Control.Monad.Reader
import Control.Monad.Morph
import Data.Diverse.Lens
import Data.Maybe
import qualified Data.Text.Lazy as TL
import GHC.Stack
import Glazier.Benign
import Glazier.Command

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

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

--------------------------------------------------------------------

-- | NB. There is no 'local' equivalent for 'LogLevelReader' because of 'Benign IO' return.
-- The whole point of 'Benign IO' is to allow the loglevel is modifiable at runtime,
-- but @local (const $ pure $ Just TRACE)@ would mean that the log level is hardcoded.
class Monad m => LogLevelReader m where
    askLogLevel :: m (Benign IO (Maybe LogLevel))

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MFunctor t, LogLevelReader m) => LogLevelReader (t m) where
    askLogLevel = lift askLogLevel

instance {-# OVERLAPPABLE #-} Monad m => LogLevelReader (ReaderT (Benign IO (Maybe LogLevel)) m) where
    askLogLevel = ask

--------------------------------------------------------------------

-- type Logger c m = (AsFacet LogLine c, MonadCommand c m, LogLevelReader m)

data LogLevel
    = TRACE
    | DEBUG
    | INFO_
    | WARN_
    -- will also print callstack
    | ERROR
    deriving (Eq, Show, Read, Ord)

data LogLine = LogLine (Benign IO (Maybe LogLevel)) LogLevel CallStack (Benign IO TL.Text)

instance Show LogLine where
    showsPrec p (LogLine _ lvl cs _) = showParen (p >= 11) $
        showString "LogLine " . shows lvl . showString " at " . showString (TL.unpack $ prettyCallStack' cs)

logLine :: (AsFacet LogLine c, MonadCommand c m, LogLevelReader m)
    => LogLevel -> CallStack -> Benign IO TL.Text
    -> m ()
logLine lvl cs m = do
    lvl' <- askLogLevel
    exec $ LogLine lvl' lvl cs m

logExec :: (Show cmd, AsFacet cmd c, AsFacet LogLine c, MonadCommand c m, LogLevelReader m)
    => LogLevel -> CallStack -> cmd -> m ()
logExec lvl cs c = do
    logLine lvl cs (pure . TL.pack $ show c)
    exec c

logExec' :: (Show (cmd c), AsFacet (cmd c) c, AsFacet LogLine c, MonadCommand c m, LogLevelReader m)
    => LogLevel -> CallStack -> cmd c -> m ()
logExec' lvl cs c = do
    logLine lvl cs (pure . TL.pack $ show c)
    exec' c

logEval :: (Show cmd, AsFacet cmd c, AsFacet LogLine c, MonadCommand c m, LogLevelReader m)
    => LogLevel -> CallStack -> ((a -> c) -> cmd) -> m a
logEval lvl cs k = delegatify $ logExec lvl cs . k

logEval' :: (Show (cmd c), AsFacet (cmd c) c, AsFacet LogLine c, MonadCommand c m, LogLevelReader m)
    => LogLevel -> CallStack -> ((a -> c) -> cmd c) -> m a
logEval' lvl cs k = delegatify $ logExec' lvl cs . k

logInvoke :: (Show (cmd c), AsFacet (cmd c) c, Functor cmd, AsFacet LogLine c, MonadCommand c m, LogLevelReader m)
    => LogLevel -> CallStack -> cmd a -> m a
logInvoke lvl cs c = logEval' lvl cs (<$> c)
