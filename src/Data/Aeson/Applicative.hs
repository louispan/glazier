{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Applicative where

import Control.Applicative
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding.Internal as E
import qualified Data.Aeson.Internal as A
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as H
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Data.Functor.Compose
-- import GHC.Stack
import Unsafe.Coerce

-- | from http://hackage.haskell.org/package/aeson-1.1.2.0/docs/src/Data-Aeson-Types-FromJSON.html
-- duplicated because it was not exported
-- Annotate parsing array with Index of parsing error
_parseIndexedJSON :: (A.Value -> A.Parser a) -> Int -> A.Value -> A.Parser a
_parseIndexedJSON p idx value = p value A.<?> A.Index idx
{-# INLINE _parseIndexedJSON #-}

-- | from http://hackage.haskell.org/package/aeson-1.1.2.0/docs/src/Data-Aeson-Types-FromJSON.html
-- modified to use a combining function
_parseIndexedJSONPair :: (a -> b -> c) -> (A.Value -> A.Parser a) -> (A.Value -> A.Parser b) -> Int -> A.Value -> A.Parser c
_parseIndexedJSONPair f keyParser valParser idx value = p value A.<?> A.Index idx
  where
    p = A.withArray "(k,v)" $ \ab ->
        let n = V.length ab
        in if n == 2
             then f <$> _parseJSONElemAtIndex keyParser 0 ab
                      <*> _parseJSONElemAtIndex valParser 1 ab
             else fail $ "cannot unpack array of length " ++
                         show n ++ " into a pair"
{-# INLINE _parseIndexedJSONPair #-}

-- | from http://hackage.haskell.org/package/aeson-1.1.2.0/docs/src/Data-Aeson-Types-FromJSON.html
-- duplicated because it was not exported
_parseJSONElemAtIndex :: (A.Value -> A.Parser a) -> Int -> V.Vector A.Value -> A.Parser a
_parseJSONElemAtIndex p idx ary = p (V.unsafeIndex ary idx) A.<?> A.Index idx
{-# INLINE _parseJSONElemAtIndex #-}

class Applicative m => MToJSON m a where
    mToEncoding :: a -> m A.Encoding

class Applicative m => MFromJSON m a where
    mParseJSON :: A.Value -> A.Parser (m a)

    mParseJSONList :: A.Value -> A.Parser (m [a])
    mParseJSONList (A.Array a)
        = fmap sequenceA
        . zipWithM (_parseIndexedJSON mParseJSON) [0..]
        . V.toList
        $ a

    mParseJSONList v = A.typeMismatch "[a]" v

mkListEncoding :: Applicative m => (a -> m A.Encoding) -> [a] -> m A.Encoding
mkListEncoding _  [] = pure E.emptyArray_
mkListEncoding f (x:xs) = (pure E.openBracket) `combine` f x `combine` commas xs `combine` (pure E.closeBracket)
  where
    combine = liftA2 (E.><)
    commas = foldr (\v vs -> (pure E.comma) `combine` f v `combine` vs) (pure $ E.Encoding mempty)
{-# INLINE mkListEncoding #-}

instance MToJSON m a => MToJSON m [a] where
    mToEncoding = mkListEncoding mToEncoding

class Applicative m => MFromJSON1 m f where
    mLiftParseJSON :: (A.Value -> A.Parser (m a)) -> (A.Value -> A.Parser (m [a])) -> A.Value -> A.Parser (m (f a))

    -- default mLiftParseJSON :: (Generic1 f, GFromJSON One (Rep1 f))
    --                       => (Value -> Parser a) -> (Value -> Parser [a]) -> Value -> Parser (f a)
    -- mLiftParseJSON = A.genericLiftParseJSON A.defaultOptions

    -- listParser :: (Value -> Parser a) -> Value -> Parser [a]
    mLiftParseJSONList :: (A.Value -> A.Parser (m a)) -> (A.Value -> A.Parser (m [a])) -> A.Value -> A.Parser (m [f a])
    mLiftParseJSONList f g v = sequenceA <$> A.listParser (mLiftParseJSON f g) v

instance Applicative m => MFromJSON1 m [] where
    mLiftParseJSON _ p' = p'

-- | Lift the standard 'parseJSON' function through the type constructor.
mParseJSON1 :: (MFromJSON1 m f, MFromJSON m a) => A.Value -> A.Parser (m (f a))
mParseJSON1 = mLiftParseJSON mParseJSON mParseJSONList

-- FIXME: MToJSON for Map
-- FIXME: MFromJSON for List

instance (Applicative m, A.FromJSONKey k, Ord k) => MFromJSON1 m (M.Map k) where
    mLiftParseJSON p _ = case A.fromJSONKey of
        A.FromJSONKeyCoerce _ -> A.withObject "Map k v" $
            H.foldrWithKey (\k v m -> getCompose $ M.insert
                <$> (Compose $ (pure . pure $ unsafeCoerce k) A.<?> A.Key k)
                <*> (Compose $ p v A.<?> A.Key k)
                <*> Compose m)
                (pure $ pure M.empty)

        A.FromJSONKeyText f -> A.withObject "Map k v" $
            H.foldrWithKey (\k v m -> getCompose $ M.insert
                <$> (Compose $ (pure . pure $ f k) A.<?> A.Key k)
                <*> (Compose $ p v A.<?> A.Key k)
                <*> Compose m)
                (pure $ pure M.empty)

        A.FromJSONKeyTextParser f -> A.withObject "Map k v" $
            H.foldrWithKey (\k v m -> getCompose $ M.insert
                <$> (Compose $ (pure <$> f k) A.<?> A.Key k)
                <*> (Compose $ p v A.<?> A.Key k)
                <*> Compose m)
                (pure $ pure M.empty)

        A.FromJSONKeyValue f -> A.withArray "Map k v" $ \arr ->
            getCompose $ M.fromList <$> (
                -- [Compose (a, b)] -> Compose [(a, b)]
                sequenceA
                . fmap Compose
                -- [Parser m (a, b)]
                . zipWith (_parseIndexedJSONPair ((<$>) . (,)) f p) [0..] . V.toList $ arr)


-- -----------------------------------------------------
-- -- FIXME: More instances, Maybe too
-- -- FIXME: Generic

instance Applicative m => MToJSON m Bool where
    mToEncoding = pure . A.toEncoding

instance Applicative m => MFromJSON m Bool where
    mParseJSON = fmap pure . A.parseJSON

instance Applicative m => MToJSON m Int where
    mToEncoding = pure . A.toEncoding

instance Applicative m => MFromJSON m Int where
    mParseJSON = fmap pure . A.parseJSON

