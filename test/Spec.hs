{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
-- Example of interpreting using polymorphic variant
-- with the help of ContT and State
module Main where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Foldable
import Data.Proxy
import Data.Tagged
import Glazier.Command
import Glazier.Command.Exec

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

-- | NB. Data.Diverse imports @Which (xs :: [Type])@
-- which is a polymorphic variant

-- | NB. Data.Diverse.Lens imports the following
--
-- @
-- class AsFacet a s where
--     facet :: Prism' s a
-- @
--
-- The polymorphic variant 'Which' has
-- AsFacet instances for all the types in the variant typelist.

----------------------------------------------
-- Commands
----------------------------------------------

-- | Define data type to encapsulate the parameters required for effects.
-- If an an effect "returns" a value (eg GetLine),
-- then the last arg is a continuation that returns the next command "command".
-- Eg (String -> cmd)
-- I don't need to derive Functor, but I do it to show
-- that the data type has the same shape as for Free Monads.
data IOEffect next
    -- PutStrLn is effect with an () return value.
    = PutStrLn String
    -- GetLine is an effect with a String return value.
    -- Requires continuation that does something with the return.
    | GetLine (String -> next)
    deriving Functor

instance Show (IOEffect c) where
    showsPrec d (PutStrLn s) = showParen (d >= 11) $ showString "PutStrLn " . shows s
    showsPrec _ (GetLine _) = showString "GetLine"

-- | Another DSL for other effects
data HelloWorldEffect
    = HelloWorld
    | ByeWorld

instance Show HelloWorldEffect where
    showsPrec _ HelloWorld = showString "HelloWorld"
    showsPrec _ ByeWorld = showString "ByeWorld"

-- | Define the sum of all variants
type AppEffects cmd = '[[cmd], Concur cmd cmd, IOEffect cmd, HelloWorldEffect]
-- | Add a newtype wrapper to allow recursive definition
newtype AppCmd = AppCmd { unAppCmd :: Which (AppEffects AppCmd)}
    deriving Show

-- | Define AsFacet instances for all types in the variant
-- UndecidableInstances!
instance (AsFacet a (Which (AppEffects AppCmd))) => AsFacet a AppCmd where
    facet = iso unAppCmd AppCmd . facet

----------------------------------------------
-- IO interpreter
----------------------------------------------

execIOEffect :: MonadIO m => (cmd -> m ()) -> IOEffect cmd -> m ()
execIOEffect _ (PutStrLn str) = liftIO $ putStrLn str
execIOEffect executor (GetLine k) = liftIO getLine >>= executor . k

execHelloWorldEffect :: MonadIO m => HelloWorldEffect -> m ()
execHelloWorldEffect HelloWorld = liftIO $ putStrLn "Hello, world!"
execHelloWorldEffect ByeWorld = liftIO $ putStrLn "Bye, world!"

-- | Combine interpreters
execEffects' ::
    ( AsFacet (IOEffect cmd) cmd
    , AsFacet HelloWorldEffect cmd
    , AsConcur cmd
    , MonadUnliftIO m
    )
    => (cmd -> m ()) -> cmd -> MaybeT m (Proxy '[[cmd], Concur cmd cmd, IOEffect cmd, HelloWorldEffect], ())
execEffects' executor c =
    maybeExec (traverse_ @[] executor) c
    `orMaybeExec` maybeExec ((>>= executor). execConcur executor) c
    `orMaybeExec` maybeExec (execIOEffect executor) c
    `orMaybeExec` maybeExec execHelloWorldEffect c

execEffects :: MonadUnliftIO m => AppCmd -> m ()
execEffects = verifyExec unAppCmd (fixExec execEffects')

----------------------------------------------
-- Test interpreter
----------------------------------------------

data Output
data Input

-- Some interpreters need to be an instance of MonadUniftIO,
-- which limits the transformer stack to ReaderT.
testIOEffect ::
    ( MonadReader r m
    , Has (Tagged Output (TVar [String])) r
    , Has (Tagged Input (TVar [String])) r
    , MonadIO m
    )
    => (cmd -> m ()) -> IOEffect cmd -> m ()
testIOEffect _ (PutStrLn str) = do
    xs <- view (hasTag @Output)
    liftIO $ atomically $ modifyTVar' xs (\xs' -> ("PutStrLn " <> show str) : xs')

testIOEffect executor (GetLine k) = do
    xs <- view (hasTag @Output)
    ys <- view (hasTag @Input)
    y <- liftIO $ atomically $ do
        ys' <- readTVar ys
        let (y, ys'') = case ys' of
                        (h : t) -> (h, t)
                        _ -> ("Unexpected GetLine!", [])
        writeTVar ys ys''
        modifyTVar' xs (\xs' -> show y <> " <- GetLine" : xs')
        pure y
    executor $ k y

testHelloWorldEffect ::
    ( MonadReader r m
    , Has (Tagged Output (TVar [String])) r
    , MonadIO m
    )
    => HelloWorldEffect -> m ()
testHelloWorldEffect HelloWorld = do
    xs <- view (hasTag @Output)
    liftIO $ atomically $ modifyTVar' xs (\xs' -> "Hello World" : xs')
testHelloWorldEffect ByeWorld = do
    xs <- view (hasTag @Output)
    liftIO $ atomically $ modifyTVar' xs (\xs' -> "Bye, World" : xs')

-- | Combine test interpreters
testEffects' ::
    ( MonadReader r m
    , Has (Tagged Output (TVar [String])) r
    , Has (Tagged Input (TVar [String])) r
    , MonadUnliftIO m
    , AsFacet (IOEffect cmd) cmd
    , AsFacet HelloWorldEffect cmd
    , AsConcur cmd
    )
    => (cmd -> m ()) -> cmd -> MaybeT m (Proxy '[[cmd], Concur cmd cmd, IOEffect cmd, HelloWorldEffect], ())
testEffects' executor c =
    maybeExec (traverse_ @[] executor) c
    `orMaybeExec` maybeExec ((>>= executor) . execConcur executor) c
    `orMaybeExec` maybeExec (testIOEffect executor) c
    `orMaybeExec` maybeExec testHelloWorldEffect c

-- | Tie testEffects_ with itself to get the final interpreter
testEffects ::
    ( MonadReader r m
    , Has (Tagged Output (TVar [String])) r
    , Has (Tagged Input (TVar [String])) r
    , MonadUnliftIO m
    ) => AppCmd -> m ()
testEffects = verifyExec unAppCmd (fixExec testEffects')

----------------------------------------------
-- programs
----------------------------------------------

ioProgram :: (AsFacet (IOEffect cmd) cmd, AsFacet [cmd] cmd) => State (DL.DList cmd) ()
ioProgram = do
    exec' $ PutStrLn "Write two things"
    evalContT $ do
        -- Use the continuation monad to compose the function to pass into GetLine
        a1 <- sequentially . eval' $ GetLine
        a2 <- sequentially . eval' $ GetLine
        -- Do something monadic/different based on the return value.
        case a1 of
            "secret" -> exec' $ PutStrLn "Easter egg!"
            _ -> do
                exec' $ PutStrLn "Write something else"
                -- more GetLine input
                b <- sequentially . eval' $ GetLine
                exec' $ PutStrLn $ "You wrote: (" <> a1 <> ", " <> a2 <> ") then " <> b

-- | using only concur
ioProgramWithOnlyConcur ::
    ( AsFacet (IOEffect cmd) cmd
    , AsConcur cmd
    -- , MonadCommand cmd m
    ) => State (DL.DList cmd) ()
ioProgramWithOnlyConcur = do
    exec' $ PutStrLn "Write two things"
    concurringly_ $ do
        -- Use the Concur monad to batch two GetLines concurrently
        a1 <- eval' $ GetLine
        a2 <- eval' $ GetLine
        -- Do something monadic/different based on the return value.
        case a1 of
            "secret" -> exec' $ PutStrLn "Easter egg!"
            _ -> do
                exec' $ PutStrLn "Write something else"
                -- more GetLine input
                b <- eval' $ GetLine
                exec' $ PutStrLn $ "You wrote: (" <> a1 <> ", " <> a2 <> ") then " <> b

-- | using concur & cont together
ioProgramWithConcur ::
    ( AsFacet (IOEffect cmd) cmd
    , AsConcur cmd) => State (DL.DList cmd) ()
ioProgramWithConcur = do
    exec' $ PutStrLn "Write two things"
    evalContT $ do
        (a1, a2) <- concurringly $ do
                -- Use the Concur monad to batch two GetLines concurrently
                -- required ApplicativeDo
                a1 <- eval' $ GetLine
                a2 <- eval' $ GetLine
                pure (a1, a2)
        -- Do something monadic/different based on the return value.
        case a1 of
            "secret" -> exec' $ PutStrLn "Easter egg!"
            _ -> do
                exec' $ PutStrLn "Write something else"
                -- more GetLine input
                b <- sequentially . eval' $ GetLine
                exec' $ PutStrLn $ "You wrote: (" <> a1 <> ", " <> a2 <> ") then " <> b

-- | Program using both effects
program ::
    ( AsFacet HelloWorldEffect cmd
    , AsFacet (IOEffect cmd) cmd
    , AsFacet [cmd] cmd) => State (DL.DList cmd) ()
program = do
    exec $ HelloWorld
    ioProgram
    exec $ ByeWorld

main :: IO ()
main = do
    -- Reduce the program to the list of commands.
    let cs :: [AppCmd]
        cs =  DL.toList $ (`execState` mempty) ioProgramWithConcur
        -- cs =  DL.toList $ (`execState` mempty) ioProgramWithOnlyConcur

    -- Shoud randomly have different results depending on which
    -- concurrent GetLine is executed first.

    -- interpret the program commands with preconfigured inputs
    is <- newTVarIO ["secret", "y", "z"]
    os <- newTVarIO ([] :: [String])
    (`runReaderT` (Tagged @Input is, Tagged @Output os)) $ testEffects $ command' @[] cs
    is' <- readTVarIO is
    os' <- readTVarIO os
    putStrLn $ "Unconsumed input: " <> show is'
    putStrLn $ "Effects executed: " <> show (reverse os')

    -- interpret the program commands interactively
    -- execEffects $ command' @[] cs
