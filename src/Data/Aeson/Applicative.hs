{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

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
import qualified Data.Text as T
import Data.Functor.Compose
import Unsafe.Coerce

-- -----------------------------------------------------
-- Utilities
-- -----------------------------------------------------

-- | from http://hackage.haskell.org/package/aeson-1.1.2.0/docs/src/Data-Aeson-Types-FromJSON.html
-- duplicated because it was not exported
-- Annotate parsing array with Index of parsing error
_parseIndexedJSON :: (A.Value -> A.Parser a) -> Int -> A.Value -> A.Parser a
_parseIndexedJSON p idx value = p value A.<?> A.Index idx

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

-- | from http://hackage.haskell.org/package/aeson-1.1.2.0/docs/src/Data-Aeson-Types-FromJSON.html
-- duplicated because it was not exported
_parseJSONElemAtIndex :: (A.Value -> A.Parser a) -> Int -> V.Vector A.Value -> A.Parser a
_parseJSONElemAtIndex p idx ary = p (V.unsafeIndex ary idx) A.<?> A.Index idx

-- -----------------------------------------------------
-- MToJSON, MFromJSON, MFromJSON1
-- -----------------------------------------------------

-- | Analogous to 'A.toJSON'
class Applicative m => MToJSON m a where
    mToEncoding :: a -> m A.Encoding

    mToEncodingList :: [a] -> m A.Encoding
    mToEncodingList = mListEncoding mToEncoding

-- | Analogous to 'A.fromJSON'
class Applicative m => MFromJSON m a where
    mParseJSON :: A.Value -> A.Parser (m a)

    mParseJSONList :: A.Value -> A.Parser (m [a])
    mParseJSONList (A.Array a)
        = fmap sequenceA
        . zipWithM (_parseIndexedJSON mParseJSON) [0..]
        . V.toList
        $ a

    mParseJSONList v = A.typeMismatch "[a]" v

-- | Analogous to 'A.fromJSON1'
class Applicative m => MFromJSON1 m f where
    mLiftParseJSON :: (A.Value -> A.Parser (m a)) -> (A.Value -> A.Parser (m [a])) -> A.Value -> A.Parser (m (f a))

    -- -- FIXME: Generic
    -- default mLiftParseJSON :: (Generic1 f, GFromJSON One (Rep1 f))
    --                       => (Value -> Parser a) -> (Value -> Parser [a]) -> Value -> Parser (f a)
    -- mLiftParseJSON = A.genericLiftParseJSON A.defaultOptions

    -- listParser :: (Value -> Parser a) -> Value -> Parser [a]
    mLiftParseJSONList :: (A.Value -> A.Parser (m a)) -> (A.Value -> A.Parser (m [a])) -> A.Value -> A.Parser (m [f a])
    mLiftParseJSONList f g v = sequenceA <$> A.listParser (mLiftParseJSON f g) v

-- | Analogous to 'A.parseJSON1'
-- Lift the standard 'parseJSON' function through the type constructor.
mParseJSON1 :: (MFromJSON1 m f, MFromJSON m a) => A.Value -> A.Parser (m (f a))
mParseJSON1 = mLiftParseJSON mParseJSON mParseJSONList

-- | Analogous to 'A.toJSON1'
class Applicative m => MToJSON1 m f where
    mLiftToEncoding :: (a -> m A.Encoding) -> ([a] -> m A.Encoding) -> f a -> m A.Encoding

    -- -- FIXME: Generic
    -- default mLiftToEncoding :: (Generic1 f, GToEncoding One (Rep1 f))
    --                        => (a -> Encoding) -> ([a] -> Encoding)
    --                        -> f a -> Encoding
    -- liftToEncoding = genericLiftToEncoding defaultOptions

    mLiftToEncodingList :: (a -> m A.Encoding) -> ([a] -> m A.Encoding) -> [f a] -> m A.Encoding
    mLiftToEncodingList f g = mListEncoding (mLiftToEncoding f g)

-- | Lift the standard 'toEncoding' function through the type constructor.
mToEncoding1 :: (MToJSON1 m f, MToJSON m a) => f a -> m A.Encoding
mToEncoding1 = mLiftToEncoding mToEncoding mToEncodingList

-- | Helper function to use with 'mLiftToEncoding'.
-- Useful when writing own 'MToJSON1' instances.
mListEncoding :: Applicative m => (a -> m A.Encoding) -> [a] -> m A.Encoding
mListEncoding _  [] = pure E.emptyArray_
mListEncoding f (x:xs) = (pure E.openBracket) `combine` f x `combine` commas xs `combine` (pure E.closeBracket)
  where
    combine = liftA2 (E.><)
    commas = foldr (\v vs -> (pure E.comma) `combine` f v `combine` vs) (pure $ E.Encoding mempty)

-- | Encode as JSON object
mDictEncoding
    :: Applicative m
    => (k -> E.Encoding' T.Text)                     -- ^ key encoding
    -> (v -> m A.Encoding)                         -- ^ value encoding
    -> (forall a. (k -> v -> a -> a) -> a -> c -> a)  -- ^ @foldrWithKey@ - indexed fold
    -> c                                              -- ^ container
    -> m A.Encoding
mDictEncoding encodeKey encodeVal foldrWithKey = foldrWithKey go (pure $ E.Encoding mempty)
  where
    combine = liftA2 (E.><)
    go k v c = encodeKV k v `combine` (pure E.comma) `combine` c
    encodeKV k v = (pure . E.retagEncoding $ encodeKey k) `combine` (pure E.colon) `combine` (E.retagEncoding <$> encodeVal v)

-- -----------------------------------------------------
-- MToJSON1, MFromJSON1 Instances
-- -----------------------------------------------------

instance Applicative m => MToJSON1 m [] where
    mLiftToEncoding _ p' = p'

instance Applicative m => MFromJSON1 m [] where
    mLiftParseJSON _ p' = p'

instance (Applicative m, A.ToJSONKey k) => MToJSON1 m (M.Map k) where
    mLiftToEncoding g _ = case A.toJSONKey of
        A.ToJSONKeyText _ f -> mDictEncoding f g M.foldrWithKey
        A.ToJSONKeyValue _ f -> mListEncoding (pairEncoding f) . M.toList
      where
        pairEncoding f (a, b) = mListEncoding id [pure $ f a, g b]

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
-- MToJSON, MFromJSON Instances
-- -----------------------------------------------------
-- -- FIXME: More instances, Maybe too

instance Applicative m => MToJSON m Bool where
    mToEncoding = pure . A.toEncoding

instance Applicative m => MFromJSON m Bool where
    mParseJSON = fmap pure . A.parseJSON

instance Applicative m => MToJSON m Int where
    mToEncoding = pure . A.toEncoding

instance Applicative m => MFromJSON m Int where
    mParseJSON = fmap pure . A.parseJSON

instance MToJSON m a => MToJSON m [a] where
    mToEncoding = mToEncoding1

instance MFromJSON m a => MFromJSON m [a] where
    mParseJSON = mParseJSON1

instance (MToJSON m a, A.ToJSONKey k) => MToJSON m (M.Map k a) where
    mToEncoding = mToEncoding1

instance (MFromJSON m a, A.FromJSONKey k, Ord k) => MFromJSON m (M.Map k a) where
    mParseJSON = mParseJSON1
