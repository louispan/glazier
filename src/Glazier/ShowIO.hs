{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.ShowIO where

import Data.IORef
import Data.Maybe
import Data.String
import System.Mem.Weak

class ShowIO str a where
    showsPrecIO :: Int  -- ^ the operator precedence of the enclosing
                        -- context (a number from @0@ to @11@).
                        -- Function application has precedence @10@.
              -> a      -- ^ the value to be converted to a 'T.Text'
              -> IO str
    showsPrecIO _ x = showIO x

    -- | A specialised variant of 'showsPrecIO', using precedence context
    -- zero, and returning an ordinary 'T.Text'.
    showIO :: a -> IO str
    showIO = showsPrecIO 0

    -- | The method 'showListIO' is provided to allow the programmer to
    -- give a specialised way of showing lists of values.
    -- For example, this is used by the predefined 'ShowIO' instance of
    -- the 'Char' type, where values of type 'String' should be shown
    -- in double quotes, rather than between square brackets.
    showListIO  :: [a] -> IO str
    default showListIO :: (Semigroup str, IsString str) => [a] -> IO str
    showListIO ls = showListIO__ showIO ls

showListIO__ :: (Semigroup str, IsString str) => (a -> IO str) -> [a] -> IO str
showListIO__ _ [] = pure "[]"
showListIO__ showx (x : xs) = (\y ys -> "[" <> y <> ys) <$> (showx x) <*> (showl xs)
  where
    showl [] = pure "]"
    showl (y : ys) = (\z zs -> "," <> z <> zs) <$> (showx y) <*> (showl ys)

showParenIO :: (Semigroup str, IsString str) => Bool -> IO str -> IO str
showParenIO False x = x
showParenIO True x = (\y -> "(" <> y <> ")") <$> x

instance {-# OVERLAPPABLE #-} (IsString str, Show a) => ShowIO str a where
    showIO = pure . fromString . show
    showsPrecIO p x = pure . fromString $ showsPrec p x ""
    showListIO xs = pure . fromString $ showList xs ""

instance {-# OVERLAPPABLE #-} ShowIO str a => ShowIO str (IORef a) where
    showIO x = readIORef x >>= showIO
    showsPrecIO p x = readIORef x >>= showsPrecIO p
    showListIO xs = traverse readIORef xs >>= showListIO

instance {-# OVERLAPPABLE #-} (IsString str, ShowIO str a) => ShowIO str (Weak a) where
    showIO x = deRefWeak x >>= maybe (pure "(null)") showIO
    showsPrecIO p x = deRefWeak x >>= maybe (pure "(null)") (showsPrecIO p)
    showListIO xs = traverse deRefWeak xs >>= showListIO . catMaybes
