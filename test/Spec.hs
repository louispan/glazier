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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- Example of interpreting using polymorphic variant
-- with the help of ContT and State
module Main where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Foldable
import Data.Proxy
import Data.Tagged
import Glazier.Command
import Glazier.Command.Exec
import Test.Hspec

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
-- type AppEffects c = '[[c], Concur c c, IOEffect c, HelloWorldEffect]
-- -- | Add a newtype wrapper to allow recursive definition
-- newtype AppCmd = AppCmd { unAppCmd :: Which (AppEffects AppCmd)}
--     deriving Show
-- -- | Define AsFac   et instances for all types in the variant
-- -- UndecidableInstances!
-- instance (AsFacet a (Which (AppEffects AppCmd))) => AsFacet a AppCmd where
--     facet = iso unAppCmd AppCmd . facet

type instance AppCmds "App" c = [[c], Concur c c, IOEffect c, HelloWorldEffect]
type AppCmd' = AppCmd "App"

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
    ( Cmd' IOEffect c
    , Cmd HelloWorldEffect c
    , Cmd'' Concur c
    , Cmd' [] c
    , MonadUnliftIO m
    )
    => (c -> m ()) -> (Proxy '[[c], Concur c c, IOEffect c, HelloWorldEffect], c -> MaybeT m ())
execEffects' executor =
    maybeExec (traverse_ @[] executor)
    `orExec` maybeExec (execConcur executor)
    `orExec` maybeExec (execIOEffect executor)
    `orExec` maybeExec execHelloWorldEffect

execEffects :: MonadUnliftIO m => AppCmd' -> m ()
execEffects = fixVerifyExec unAppCmd execEffects'

----------------------------------------------
-- Test interpreter
----------------------------------------------

-- Some interpreters need to be an instance of MonadUniftIO,
-- which limits the transformer stack to ReaderT.
testIOEffect ::
    ( MonadReader r m
    , Has (Tagged "Output" (TVar [String])) r
    , Has (Tagged "Input" (TVar [String])) r
    , MonadIO m
    )
    => (cmd -> m ()) -> IOEffect cmd -> m ()
testIOEffect _ (PutStrLn str) = do
    xs <- view (hasTag @"Output")
    liftIO $ atomically $ modifyTVar' xs (\xs' -> ("PutStrLn " <> show str) : xs')

testIOEffect executor (GetLine k) = do
    xs <- view (hasTag @"Output")
    ys <- view (hasTag @"Input")
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
    , Has (Tagged "Output" (TVar [String])) r
    , MonadIO m
    )
    => HelloWorldEffect -> m ()
testHelloWorldEffect HelloWorld = do
    xs <- view (hasTag @"Output")
    liftIO $ atomically $ modifyTVar' xs (\xs' -> "Hello World" : xs')
testHelloWorldEffect ByeWorld = do
    xs <- view (hasTag @"Output")
    liftIO $ atomically $ modifyTVar' xs (\xs' -> "Bye, World" : xs')

-- | Combine test interpreters
testEffects' ::
    ( MonadReader r m
    , Has (Tagged "Output" (TVar [String])) r
    , Has (Tagged "Input" (TVar [String])) r
    , MonadUnliftIO m
    , Cmd' IOEffect c
    , Cmd HelloWorldEffect c
    , Cmd'' Concur c
    , Cmd' [] c
    )
    => (c -> m ()) -> (Proxy '[[c], Concur c c, IOEffect c, HelloWorldEffect], c -> MaybeT m ())
testEffects' executor =
    maybeExec (traverse_ @[] executor)
    `orExec` maybeExec (execConcur executor)
    `orExec` maybeExec (testIOEffect executor)
    `orExec` maybeExec testHelloWorldEffect

-- | Tie testEffects_ with itself to get the final interpreter
testEffects ::
    ( MonadReader r m
    , Has (Tagged "Output" (TVar [String])) r
    , Has (Tagged "Input" (TVar [String])) r
    , MonadUnliftIO m
    ) => AppCmd' -> m ()
testEffects = fixVerifyExec unAppCmd testEffects'

----------------------------------------------
-- programs
----------------------------------------------

ioProgram :: (Cmd' IOEffect c, Cmd' [] c) => Program c ()
ioProgram = do
    exec' $ PutStrLn "Write two things"
    evalContT $ do
        -- Use the continuation monad to compose the function to pass into GetLine
        a1 <- sequentially . delegatify $ exec' . GetLine
        a2 <- sequentially . delegatify $ exec' . GetLine
        -- Do something monadic/different based on the return value.
        case a1 of
            "secret" -> exec' $ PutStrLn "Easter egg!"
            _ -> do
                exec' $ PutStrLn "Write something else"
                -- more GetLine input
                b <- sequentially . delegatify $ exec' . GetLine
                exec' $ PutStrLn $ "You wrote: (" <> a1 <> ", " <> a2 <> ") then " <> b

-- | using only concur
-- concurringly_ is ok to use by @Program c ()@
ioProgramWithOnlyConcur ::
    ( Cmd' IOEffect c
    , Cmd'' Concur c
    , Cmd' [] c
    ) => Program c ()
ioProgramWithOnlyConcur = do
    exec' $ PutStrLn "Write two things"
    concurringly_ $ do
        -- Use the Concur monad to batch two GetLines concurrently
        a1 <- delegatify $ exec' . GetLine
        a2 <- delegatify $ exec' . GetLine
        -- Do something monadic/different based on the return value.
        evalContT $ sequentially $ do
            -- exec' $ PutStrLn "Foobar"
            case a1 of
                "secret" -> do
                    exec' $ PutStrLn "Easter egg!"
                    -- exec' $ PutStrLn "easter"
                _ -> do
                    -- NB everything is threaded!
                    exec' $ PutStrLn "Write something else"
                    -- more GetLine input
                    b <- delegatify $ exec' . GetLine
                    exec' $ PutStrLn $ "You wrote: (" <> a1 <> ", " <> a2 <> ") then " <> b
                    -- exec' $ PutStrLn "bar"

-- | using concur & cont together
-- evalContT is required because @Program c ()@ is not a MonadDelegate by itself
ioProgramWithConcur ::
    ( Cmd' IOEffect c
    , Cmd'' Concur c
    , Cmd' [] c) => Program c ()
ioProgramWithConcur = do
    exec' $ PutStrLn "Write two things"
    evalContT $ do
        (a1, a2) <- concurringly $ do
                -- Use the Concur monad to batch two GetLines concurrently
                -- requires ApplicativeDo
                a1 <- delegatify $ exec' . GetLine
                a2 <- delegatify $ exec' . GetLine
                pure (a1, a2)
        -- Do something monadic/different based on the return value.
        case a1 of
            "secret" -> exec' $ PutStrLn "Easter egg!"
            _ -> do
                exec' $ PutStrLn "Write something else"
                -- more GetLine input, but sequentially
                b <- sequentially . delegatify $ exec' . GetLine
                exec' $ PutStrLn $ "You wrote: (" <> a1 <> ", " <> a2 <> ") then " <> b

-- | Program using both effects
program ::
    ( Cmd HelloWorldEffect c
    , Cmd' IOEffect c
    , Cmd' [] c) => Program c ()
program = do
    exec $ HelloWorld
    ioProgram
    exec $ ByeWorld

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "ioProgram" $ do
        it "always produce the same ouput for the given input" $ do
            -- Reduce the program to the list of commands.
            let cs :: [AppCmd']
                cs =  DL.toList $ execProgram' ioProgram

            -- Shoud randomly have different results depending on which
            -- concurrent GetLine is executed first.
            replicateM_ 1000 $ do
                (is, os) <- specProgram cs
                (length is :: Int) `shouldSatisfy` (== 1) -- is always just 'z' left
                (length os :: Int) `shouldSatisfy` (== 4)

    describe "ioProgramWithOnlyConcur" $ do
        it "always produce same output the initial input, then may produce the different ouput for last input" $ do
            -- Reduce the program to the list of commands.
            let cs :: [AppCmd']
                cs =  DL.toList $ execProgram' ioProgramWithOnlyConcur

            -- Shoud randomly have different results depending on which
            -- concurrent GetLine is executed first.
            replicateM_ 1000 $ do
                (is, os) <- specProgram cs
                (length is :: Int) `shouldSatisfy` (<= 1) -- could be fully consumed or has just 'z' left
                (length os :: Int) `shouldSatisfy` (\a -> a == 4 || a == 6)


    describe "ioProgramWithConcur" $ do
        it "is similar as ioProgramWithOnlyConcur but uses concurringly_ to avoid evalContT" $ do
            -- Reduce the program to the list of commands.
            let cs :: [AppCmd']
                cs =  DL.toList $ execProgram' ioProgramWithConcur

            -- Shoud randomly have different results depending on which
            -- concurrent GetLine is executed first.
            replicateM_ 1000 $ do
                (is, os) <- specProgram cs
                (length is :: Int) `shouldSatisfy` (<= 1) -- could be fully consumed or has just 'z' left
                (length os :: Int) `shouldSatisfy` (\a -> a == 4 || a == 6)

    -- describe "execEffects" $ do
    --     it "uncomment to run the program commands interactively" $ do
    --         -- interpret the program commands interactively
    --         let cs :: [AppCmd']
    --             cs =  DL.toList $ execProgram' ioProgramWithConcur
    --         execEffects $ command' @[] cs :: IO ()


specProgram :: [AppCmd'] -> IO ([String], [String])
specProgram cs = do
    -- interpret the program commands with preconfigured inputs
    is <- newTVarIO ["secret", "y", "z"]
    os <- newTVarIO ([] :: [String])
    (`runReaderT` (Tagged @"Input" is, Tagged @"Output" os)) $ testEffects $ command' @[] cs
    is' <- readTVarIO is
    os' <- readTVarIO os
    -- putStrLn $ "Unconsumed input: " <> show is'
    let os'' = reverse os'
    -- putStrLn $ "Effects executed: " <> show os''
    pure (is', os'')
