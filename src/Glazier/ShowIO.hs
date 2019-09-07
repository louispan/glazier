{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.ShowIO where

import Data.Maybe
import Data.IORef
import qualified Data.Text as T
import System.Mem.Weak


class ShowIO a where
    showsPrecIO :: Int  -- ^ the operator precedence of the enclosing
                        -- context (a number from @0@ to @11@).
                        -- Function application has precedence @10@.
              -> a      -- ^ the value to be converted to a 'T.Text'
              -> IO T.Text
    showsPrecIO _ x = showIO x

    -- | A specialised variant of 'showsPrecIO', using precedence context
    -- zero, and returning an ordinary 'T.Text'.
    showIO :: a -> IO T.Text
    showIO = showsPrecIO 0

    -- | The method 'showListIO' is provided to allow the programmer to
    -- give a specialised way of showing lists of values.
    -- For example, this is used by the predefined 'ShowIO' instance of
    -- the 'Char' type, where values of type 'String' should be shown
    -- in double quotes, rather than between square brackets.
    showListIO  :: [a] -> IO T.Text
    showListIO ls = showListIO__ showIO ls

showListIO__ :: (a -> IO T.Text) -> [a] -> IO T.Text
showListIO__ _ [] = pure "[]"
showListIO__ showx (x : xs) = (\y ys -> '[' `T.cons` y <> ys) <$> (showx x) <*> (showl xs)
  where
    showl [] = pure "]"
    showl (y : ys) = (\z zs -> ',' `T.cons` z <> zs) <$> (showx y) <*> (showl ys)

showParenIO :: Bool -> IO T.Text -> IO T.Text
showParenIO False x = x
showParenIO True x = (\y -> '(' `T.cons` y `T.snoc` ')') <$> x

instance {-# OVERLAPPABLE #-} Show a => ShowIO a where
    showIO = pure . T.pack . show
    showsPrecIO p x = pure . T.pack $ showsPrec p x ""
    showListIO xs = pure . T.pack $ showList xs ""

instance {-# OVERLAPPABLE #-} ShowIO a => ShowIO (IORef a) where
    showIO x = readIORef x >>= showIO
    showsPrecIO p x = readIORef x >>= showsPrecIO p
    showListIO xs = traverse readIORef xs >>= showListIO

instance {-# OVERLAPPABLE #-} ShowIO a => ShowIO (Weak a) where
    showIO x = deRefWeak x >>= maybe (pure "(null)") showIO
    showsPrecIO p x = deRefWeak x >>= maybe (pure "(null)") (showsPrecIO p)
    showListIO xs = traverse deRefWeak xs >>= showListIO . catMaybes