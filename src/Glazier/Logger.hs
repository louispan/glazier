{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.Logger where

import Data.Diverse.Lens
import GHC.Stack
import Glazier.Benign
import Glazier.Command
import qualified Data.Text.Lazy as TL

type AsLogger c =
    ( AsFacet [c] c -- implicity required by 'MonadCodify'
    , AsFacet Logger c
    )

instance Show Logger where
    showsPrec p (Logger lvl stk _) = showParen (p >= 11) $
        showString "Logger " . shows lvl . showString " at " . showString (prettyCallStack stk)

data LogLevel
    -- console.info
    = Trace
    -- console.info
    | Debug
    -- console.info
    | Info
    -- console.warn
    | Warn
    -- console.error, will also print callstack
    | Error
    deriving (Eq, Show, Read, Ord)

data Logger = Logger LogLevel CallStack (Benign IO TL.Text)

-- | @console.error("ERROR " + msg + "[function@path/to/src.hs:line:col in package:Module.Name]")
-- Will print the full callstack, not just the first level.
log_ :: (MonadCommand c m, AsLogger c) => LogLevel -> CallStack -> Benign IO TL.Text -> m ()
log_ lvl cs m = exec $ Logger lvl cs m

-- -- | @console.info("TRACE " + msg + "[function@path/to/src.hs:line:col in package:Module.Name]")
-- -- NB. Reactor commands are automatically logged at trace level.
-- logTrace :: (MonadCommand c m, AsLogger c) => LogLevel -> CallStack -> Benign IO TL.Text -> m ()
-- logTrace = log_ Trace

-- -- | @console.info("DEBUG " + msg + "[function@path/to/src.hs:line:col in package:Module.Name]")
-- logDebug :: (MonadCommand c m, AsLogger c) => LogLevel -> CallStack -> Benign IO TL.Text -> m ()
-- logDebug = log_ Debug

-- -- | @console.info("INFO  " + msg + "[function@path/to/src.hs:line:col in package:Module.Name]")
-- logInfo :: (MonadCommand c m, AsLogger c) => LogLevel -> CallStack -> Benign IO TL.Text -> m ()
-- logInfo = log_ Info

-- -- | @console.warn("WARN  " + msg + "[function@path/to/src.hs:line:col in package:Module.Name]")
-- logWarn :: (MonadCommand c m, AsLogger c) => LogLevel -> CallStack -> Benign IO TL.Text -> m ()
-- logWarn = log_ Warn

-- -- | @console.error("ERROR " + msg + "[function@path/to/src.hs:line:col in package:Module.Name]")
-- -- Will print the full callstack, not just the first level.
-- logError :: (MonadCommand c m, AsLogger c) => LogLevel -> CallStack -> Benign IO TL.Text -> m ()
-- logError = log_ Error

logExec :: (Show cmd, AsFacet cmd c, MonadCommand c m, AsLogger c)
    => LogLevel -> CallStack -> cmd -> m ()
logExec lvl cs c = do
    log_ lvl cs (pure . TL.pack $ show c)
    exec c

logExec' :: (Show (cmd c), AsFacet (cmd c) c, MonadCommand c m, AsLogger c)
    => LogLevel -> CallStack -> cmd c -> m ()
logExec' lvl cs c = do
    log_ lvl cs (pure . TL.pack $ show c)
    exec' c

logEval :: (Show cmd, AsFacet cmd c, MonadCommand c m, AsLogger c)
    => LogLevel -> CallStack -> ((a -> c) -> cmd) -> m a
logEval lvl cs k = delegate $ logExec lvl cs . k

logEval' :: (Show (cmd c), AsFacet (cmd c) c, MonadCommand c m, AsLogger c)
    => LogLevel -> CallStack -> ((a -> c) -> cmd c) -> m a
logEval' lvl cs k = delegate $ logExec' lvl cs . k

logInvoke :: (Show (cmd c), AsFacet (cmd c) c, MonadCommand c m, AsLogger c, Functor cmd)
    => LogLevel -> CallStack -> cmd a -> m a
logInvoke lvl cs c = logEval' lvl cs (<$> c)
