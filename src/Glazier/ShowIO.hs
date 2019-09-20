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


-- | The @shows@ functions return a function that prepends the
-- output 'String' to an existing 'String'.  This allows constant-time
-- concatenation of results using function composition.
type ShowStr str = str -> str

class ShowIO str a where
    showsPrecIO :: Int  -- ^ the operator precedence of the enclosing
                        -- context (a number from @0@ to @11@).
                        -- Function application has precedence @10@.
              -> a      -- ^ the value to be converted to a 'T.Text'
              -> IO (ShowStr str)
    default showsPrecIO :: (Semigroup str) => Int -> a -> IO (ShowStr str)
    showsPrecIO _ x = showStr <$> showIO x

    -- | A specialised variant of 'showsPrecIO', using precedence context
    -- zero, and returning an ordinary 'T.Text'.
    showIO :: a -> IO str
    default showIO :: (IsString str) => a -> IO str
    showIO x = (\f -> f (fromString "")) <$> showsPrecIO 0 x

    -- | The method 'showListIO' is provided to allow the programmer to
    -- give a specialised way of showing lists of values.
    -- For example, this is used by the predefined 'ShowIO' instance of
    -- the 'Char' type, where values of type 'String' should be shown
    -- in double quotes, rather than between square brackets.
    showListIO  :: [a] -> IO (ShowStr str)
    default showListIO :: (Semigroup str, IsString str) => [a] -> IO (ShowStr str)
    showListIO ls = showListIO__ showsIO ls

showListIO__ :: (Semigroup str, IsString str) => (a -> IO (ShowStr str)) -> [a] -> IO (ShowStr str)
showListIO__ _ [] = pure (showStr "[]")
showListIO__ showx (x : xs) = (\y ys -> showStr "[" . y . ys) <$> (showx x) <*> (showl xs)
  where
    showl [] = pure (showStr "]")
    showl (y : ys) = (\z zs -> showStr "," . z . zs) <$> (showx y) <*> (showl ys)

showParenIO :: (Semigroup str, IsString str) => Bool -> IO (ShowStr str) -> IO (ShowStr str)
showParenIO False x = x
showParenIO True x = (\y -> showStr "(" . y . showStr ")") <$> x

-- | equivalent to 'showsPrecIO' with a precedence of 0.
showsIO :: (ShowIO str a) => a -> IO (ShowStr str)
showsIO = showsPrecIO 0

showsStr :: (IsString str, Semigroup str, Show a) => a -> ShowStr str
showsStr = showFromStr . show

-- | utility function converting a 'String' to a show function that
-- simply prepends the string unchanged.
showStr :: Semigroup str => str -> ShowStr str
showStr = (<>)

showFromStr :: (IsString str, Semigroup str) => String -> ShowStr str
showFromStr = showStr . fromString

instance {-# OVERLAPPABLE #-} (IsString str, Semigroup str, Show a) => ShowIO str a where
    showIO = pure . fromString . show
    showsPrecIO p x = pure . showStr . fromString $ showsPrec p x ""
    showListIO xs = pure . showStr . fromString $ showList xs ""

instance {-# OVERLAPPABLE #-} ShowIO str a => ShowIO str (IORef a) where
    showIO x = readIORef x >>= showIO
    showsPrecIO p x = readIORef x >>= showsPrecIO p
    showListIO xs = traverse readIORef xs >>= showListIO

instance {-# OVERLAPPABLE #-} (IsString str, Semigroup str, ShowIO str a) => ShowIO str (Weak a) where
    showIO x = deRefWeak x >>= maybe (pure "(null)") showIO
    showsPrecIO p x = deRefWeak x >>= maybe (pure $ showStr "(null)") (showsPrecIO p)
    showListIO xs = traverse deRefWeak xs >>= showListIO . catMaybes
