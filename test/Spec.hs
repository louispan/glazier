{-# LANGUAGE ApplicativeDo #-}
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

import Control.Applicative
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Foldable
import Data.Tagged
import Glazier.Command
import Glazier.Command.Exec

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
type AppCmd' cmd = Which '[(), [cmd], Concur cmd cmd, IOEffect cmd, HelloWorldEffect]
-- | Add a newtype wrapper to allow recursive definition
newtype AppCmd = AppCmd { unAppCmd :: AppCmd' AppCmd}
    deriving Show

-- | Define AsFacet instances for all types in the variant
-- UndecidableInstances!
instance (AsFacet a (AppCmd' AppCmd)) => AsFacet a AppCmd where
    facet = iso unAppCmd AppCmd . facet

----------------------------------------------
-- IO interpreter
----------------------------------------------

execIOEffect :: MonadIO m => (cmd -> m ()) -> IOEffect cmd -> m ()
execIOEffect _ (PutStrLn str) = liftIO $ putStrLn str
execIOEffect exec (GetLine k) = liftIO getLine >>= exec . k

execHelloWorldEffect :: MonadIO m => HelloWorldEffect -> m ()
execHelloWorldEffect HelloWorld = liftIO $ putStrLn "Hello, world!"
execHelloWorldEffect ByeWorld = liftIO $ putStrLn "Bye, world!"

-- | Combine interpreters
execEffects_ ::
    ( AsFacet (IOEffect cmd) cmd
    , AsFacet HelloWorldEffect cmd
    , AsConcur cmd
    , MonadUnliftIO m
    )
    => (cmd -> m ()) -> cmd -> MaybeT m ()
execEffects_ exec c =
    maybeExec (traverse_ @[] exec) c
    <|> maybeExec ((>>= exec). execConcur exec) c
    <|> maybeExec (execIOEffect exec) c
    <|> maybeExec execHelloWorldEffect c

-- | Tie execEffects_ with itself to get the final interpreter
execEffects ::
    ( AsFacet (IOEffect cmd) cmd
    , AsFacet HelloWorldEffect cmd
    , AsConcur cmd
    , Show cmd
    , MonadUnliftIO m
    )
    => cmd -> m ()
execEffects = void . runMaybeT . execEffects_ execEffects

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
    xs <- view (pieceTag' @Output)
    liftIO $ atomically $ modifyTVar' xs (\xs' -> ("PutStrLn " <> show str) : xs')

testIOEffect exec (GetLine k) = do
    xs <- view (pieceTag' @Output)
    ys <- view (pieceTag' @Input)
    y <- liftIO $ atomically $ do
        ys' <- readTVar ys
        let (y, ys'') = case ys' of
                        (h : t) -> (h, t)
                        _ -> ("Unexpected GetLine!", [])
        writeTVar ys ys''
        modifyTVar' xs (\xs' -> show y <> " <- GetLine" : xs')
        pure y
    exec $ k y

testHelloWorldEffect ::
    ( MonadReader r m
    , Has (Tagged Output (TVar [String])) r
    , MonadIO m
    )
    => HelloWorldEffect -> m ()
testHelloWorldEffect HelloWorld = do
    xs <- view (pieceTag' @Output)
    liftIO $ atomically $ modifyTVar' xs (\xs' -> "Hello World" : xs')
testHelloWorldEffect ByeWorld = do
    xs <- view (pieceTag' @Output)
    liftIO $ atomically $ modifyTVar' xs (\xs' -> "Bye, World" : xs')

-- | Combine test interpreters
testEffects_ ::
    ( MonadReader r m
    , Has (Tagged Output (TVar [String])) r
    , Has (Tagged Input (TVar [String])) r
    , MonadUnliftIO m
    , AsFacet (IOEffect cmd) cmd
    , AsFacet HelloWorldEffect cmd
    , AsConcur cmd
    )
    => (cmd -> m ()) -> cmd -> MaybeT m ()
testEffects_ exec c =
    maybeExec (traverse_ @[] exec) c
    <|> maybeExec ((>>= exec) . execConcur exec) c
    <|> maybeExec (testIOEffect exec) c
    <|> maybeExec testHelloWorldEffect c

-- -- | Combine test interpreters
-- testEffects2_ :: forall m r.
--     ( MonadReader r m
--     , Has (Tagged Output (TVar [String])) r
--     , Has (Tagged Input (TVar [String])) r
--     , MonadUnliftIO m
--     )
--     => (AppCmd -> m ()) -> AppCmd -> m ()
-- testEffects2_ exec (AppCmd c) =
--     switch c (
--         cases ((traverse_ @[] exec :: [AppCmd] -> m ())
--             ./ (execUnit :: () -> m ())
--             ./ (execConcurCmd exec :: ConcurCmd AppCmd -> m ())
--             ./ (testIOEffect exec :: IOEffect AppCmd -> m ())
--             ./ (testHelloWorldEffect :: HelloWorldEffect -> m ())
--             ./ nil))

-- | Tie testEffects_ with itself to get the final interpreter
testEffects ::
    ( MonadReader r m
    , Has (Tagged Output (TVar [String])) r
    , Has (Tagged Input (TVar [String])) r
    , MonadUnliftIO m
    , AsFacet (IOEffect cmd) cmd
    , AsFacet HelloWorldEffect cmd
    , AsConcur cmd
    )
    => cmd -> m ()
testEffects = void . runMaybeT . testEffects_ testEffects

----------------------------------------------
-- programs
----------------------------------------------

ioProgram :: (AsFacet (IOEffect cmd) cmd, AsFacet [cmd] cmd) => State (DL.DList cmd) ()
ioProgram = do
    postCmd' $ PutStrLn "Write two things"
    evalContT $ do
        -- Use the continuation monad to compose the function to pass into GetLine
        a1 <- sequel $ postCmd' . GetLine
        a2 <- sequel $ postCmd' . GetLine
        -- Do something monadic/different based on the return value.
        case a1 of
            "secret" -> postCmd' $ PutStrLn "Easter egg!"
            _ -> do
                postCmd' $ PutStrLn "Write something else"
                -- more GetLine input
                b <- sequel $ postCmd' . GetLine
                postCmd' $ PutStrLn $ "You wrote: (" <> a1 <> ", " <> a2 <> ") then " <> b

-- | using only concur
ioProgramWithOnlyConcur ::
    ( AsFacet (IOEffect cmd) cmd
    , AsConcur cmd) => State (DL.DList cmd) ()
ioProgramWithOnlyConcur = do
    postCmd' $ PutStrLn "Write two things"
    concurringly_ $ do
        -- Use the Concur monad to batch two GetLines concurrently
        a1 <- outcome $ postCmd' . GetLine
        a2 <- outcome $ postCmd' . GetLine
        -- Do something monadic/different based on the return value.
        case a1 of
            "secret" -> postCmd' $ PutStrLn "Easter egg!"
            _ -> do
                postCmd' $ PutStrLn "Write something else"
                -- more GetLine input
                b <- outcome $ postCmd' . GetLine
                postCmd' $ PutStrLn $ "You wrote: (" <> a1 <> ", " <> a2 <> ") then " <> b

-- | using concur & cont together
ioProgramWithConcur ::
    ( AsFacet (IOEffect cmd) cmd
    , AsConcur cmd) => State (DL.DList cmd) ()
ioProgramWithConcur = do
    postCmd' $ PutStrLn "Write two things"
    evalContT $ do
        (a1, a2) <- concurringly $ do
                -- Use the Concur monad to batch two GetLines concurrently
                -- required ApplicativeDo
                a1 <- outcome $ postCmd' . GetLine
                a2 <- outcome $ postCmd' . GetLine
                pure (a1, a2)
        -- Do something monadic/different based on the return value.
        case a1 of
            "secret" -> postCmd' $ PutStrLn "Easter egg!"
            _ -> do
                postCmd' $ PutStrLn "Write something else"
                -- more GetLine input
                b <- sequel $ postCmd' . GetLine
                postCmd' $ PutStrLn $ "You wrote: (" <> a1 <> ", " <> a2 <> ") then " <> b

-- | Program using both effects
program ::
    ( AsFacet HelloWorldEffect cmd
    , AsFacet (IOEffect cmd) cmd
    , AsFacet [cmd] cmd) => State (DL.DList cmd) ()
program = do
    postCmd $ HelloWorld
    ioProgram
    postCmd $ ByeWorld

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
