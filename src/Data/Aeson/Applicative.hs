{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Applicative where

import Control.Applicative
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding.Internal as E
import qualified Data.Aeson.Internal as A
import qualified Data.Aeson.Types as A
import Data.Functor.Compose
import qualified Data.HashMap.Strict as H
import qualified Data.List as DL
import qualified Data.Map.Strict as M
import Data.Tagged
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified GHC.Generics as G
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

    default mToEncoding :: (G.Generic a, GMToEncoding m A.Zero (G.Rep a)) => a -> m A.Encoding
    mToEncoding = genericMToEncoding A.defaultOptions

    mToEncodingList :: [a] -> m A.Encoding
    mToEncodingList = mListEncoding mToEncoding

-- | Analogous to 'A.fromJSON'
class Applicative m => MFromJSON m a where
    mParseJSON :: A.Value -> A.Parser (m a)

    default mParseJSON :: (G.Generic a, GMFromJSON m A.Zero (G.Rep a)) => A.Value -> A.Parser (m a)
    mParseJSON = genericMParseJSON A.defaultOptions

    mParseJSONList :: A.Value -> A.Parser (m [a])
    mParseJSONList (A.Array a)
        = fmap sequenceA
        . zipWithM (_parseIndexedJSON mParseJSON) [0..]
        . V.toList
        $ a

    mParseJSONList v = A.typeMismatch "[a]" v


-- -----------------------------------------------------
-- MToJSON1, MFromJSON1
-- -----------------------------------------------------

-- | Analogous to 'A.fromJSON1'
class Applicative m => MFromJSON1 m f where
    mLiftParseJSON :: (A.Value -> A.Parser (m a)) -> (A.Value -> A.Parser (m [a])) -> A.Value -> A.Parser (m (f a))

    default mLiftParseJSON :: (G.Generic1 f, GMFromJSON m A.One (G.Rep1 f))
                          => (A.Value -> A.Parser (m a)) -> (A.Value -> A.Parser (m [a])) -> A.Value -> A.Parser (m (f a))
    mLiftParseJSON = genericMLiftParseJSON A.defaultOptions

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

    default mLiftToEncoding :: (G.Generic1 f, GMToEncoding m A.One (G.Rep1 f))
                           => (a -> m A.Encoding) -> ([a] -> m A.Encoding)
                           -> f a -> m A.Encoding
    mLiftToEncoding = genericMLiftToEncoding A.defaultOptions

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
-- Generic
-- -----------------------------------------------------

-- | Class of generic representation types that can be converted to
-- a JSON 'Encoding'.
class GMToEncoding m arity f where
    -- | This method (applied to 'defaultOptions') can be used as the
    -- default generic implementation of 'toEncoding' (if the @arity@ is 'Zero')
    -- or 'liftToEncoding' (if the @arity@ is 'One').
    gMToEncoding :: A.Options -> MToArgs m A.Encoding arity a -> f a -> m A.Encoding

-- | A 'ToArgs' value either stores nothing (for 'ToJSON') or it stores the two
-- function arguments that encode occurrences of the type parameter (for
-- 'ToJSON1').
data MToArgs m res arity a where
    MNoToArgs :: MToArgs m res A.Zero a
    MTo1Args  :: (a -> m res) -> ([a] -> m res) -> MToArgs m res A.One a

-- | A configurable generic JSON encoder. This function applied to
-- 'defaultOptions' is used as the default for 'mToEncoding' when the type
-- is an instance of 'Generic'.
genericMToEncoding :: (G.Generic a, GMToEncoding m A.Zero (G.Rep a))
                  => A.Options -> a -> m A.Encoding
genericMToEncoding opts = gMToEncoding opts MNoToArgs . G.from

-- | A configurable generic JSON encoder. This function applied to
-- 'defaultOptions' is used as the default for 'liftToEncoding' when the type
-- is an instance of 'Generic1'.
genericMLiftToEncoding :: (G.Generic1 f, GMToEncoding m A.One (G.Rep1 f))
                      => A.Options -> (a -> m A.Encoding) -> ([a] -> m A.Encoding)
                      -> f a -> m A.Encoding
genericMLiftToEncoding opts te tel = gMToEncoding opts (MTo1Args te tel) . G.from1

-- | Class of generic representation types that can be converted from JSON.
class Applicative m => GMFromJSON m arity f where
    -- | This method (applied to 'defaultOptions') is used as the
    -- default generic implementation of 'parseJSON' (if the @arity@ is 'Zero')
    -- or 'liftParseJSON' (if the @arity@ is 'One').
    gMParseJSON :: A.Options -> MFromArgs m arity a -> A.Value -> A.Parser (m (f a))

-- | A 'FromArgs' value either stores nothing (for 'FromJSON') or it stores the
-- two function arguments that decode occurrences of the type parameter (for
-- 'FromJSON1').
data MFromArgs m arity a where
    MNoFromArgs :: MFromArgs m A.Zero a
    MFrom1Args  :: (A.Value -> A.Parser (m a)) -> (A.Value -> A.Parser (m [a])) -> MFromArgs m A.One a

-- | A configurable generic JSON decoder. This function applied to
-- 'defaultOptions' is used as the default for 'parseJSON' when the
-- type is an instance of 'Generic'.
genericMParseJSON :: (G.Generic a, GMFromJSON m A.Zero (G.Rep a))
                 => A.Options -> A.Value -> A.Parser (m a)
genericMParseJSON opts = fmap (fmap G.to) . gMParseJSON opts MNoFromArgs

-- | A configurable generic JSON decoder. This function applied to
-- 'defaultOptions' is used as the default for 'liftParseJSON' when the
-- type is an instance of 'Generic1'.
genericMLiftParseJSON :: (G.Generic1 f, GMFromJSON m A.One (G.Rep1 f))
                     => A.Options -> (A.Value -> A.Parser (m a)) -> (A.Value -> A.Parser (m [a]))
                     -> A.Value -> A.Parser (m (f a))
genericMLiftParseJSON opts pj pjl = fmap (fmap G.to1) . gMParseJSON opts (MFrom1Args pj pjl)

--------------------------------------------------------------------------------
-- From unexported Data/Aeson/Types/Generic.hs
--------------------------------------------------------------------------------

class IsRecord (f :: * -> *) (isRecord :: k) | f -> isRecord
  where
    isUnary :: f a -> Bool
    isUnary = const True

instance (IsRecord f isRecord) => IsRecord (f G.:*: g) isRecord
  where isUnary = const False
#if MIN_VERSION_base(4,9,0)
instance {-# OVERLAPPING #-} IsRecord (G.M1 G.S ('G.MetaSel 'Nothing u ss ds) f) 'False
#else
instance {-# OVERLAPPING #-} IsRecord (G.M1 G.S G.NoSelector f) 'False
#endif
instance (IsRecord f isRecord) => IsRecord (G.M1 G.S c f) isRecord
instance IsRecord (G.K1 i c) 'True
instance IsRecord G.Par1 'True
instance IsRecord (G.Rec1 f) 'True
instance IsRecord (f G.:.: g) 'True
instance IsRecord G.U1 'False
  where isUnary = const False

--------------------------------------------------------------------------------

class AllNullary (f :: * -> *) allNullary | f -> allNullary

instance ( AllNullary a allNullaryL
         , AllNullary b allNullaryR
         , And allNullaryL allNullaryR allNullary
         ) => AllNullary (a G.:+: b) allNullary
instance AllNullary a allNullary => AllNullary (G.M1 i c a) allNullary
instance AllNullary (a G.:*: b) 'False
instance AllNullary (a G.:.: b) 'False
instance AllNullary (G.K1 i c) 'False
instance AllNullary G.Par1 'False
instance AllNullary (G.Rec1 f) 'False
instance AllNullary G.U1 'True

-- -----------------------------------------------------

class    And bool1 bool2 bool3 | bool1 bool2 -> bool3

instance And 'True  'True  'True
instance And 'False 'False 'False
instance And 'False 'True  'False
instance And 'True  'False 'False

--------------------------------------------------------------------------------
-- | From unexported Data/Aeson/Types/ToJSON.hs
--------------------------------------------------------------------------------

-- | Get the name of the constructor of a sum datatype.
class GetConName f where
    getConName :: f a -> String

instance (GetConName a, GetConName b) => GetConName (a G.:+: b) where
    getConName (G.L1 x) = getConName x
    getConName (G.R1 x) = getConName x

instance (G.Constructor c) => GetConName (G.C1 c a) where
    getConName = G.conName

-- -----------------------------------------------------

class RecordMToEncoding m arity f where
    -- 1st element: whole thing
    -- 2nd element: in case the record has only 1 field, just the value
    --              of the field (without the key); 'Nothing' otherwise
    recordMToEncoding :: A.Options -> MToArgs m A.Encoding arity a
                     -> f a -> (m A.Encoding, Maybe (m A.Encoding))

instance ( Applicative m
         , RecordMToEncoding m arity a
         , RecordMToEncoding m arity b
         ) => RecordMToEncoding m arity (a G.:*: b) where
    recordMToEncoding opts targs (a G.:*: b) | A.omitNothingFields opts =
      ( fmap (E.econcat . DL.intersperse E.comma .
        filter (not . E.nullEncoding)) $
        sequenceA
        [ fst (recordMToEncoding opts targs a)
        , fst (recordMToEncoding opts targs b) ]
      , Nothing)
    recordMToEncoding opts targs (a G.:*: b) =
      (fst (recordMToEncoding opts targs a) `combine` (pure E.comma) `combine`
       fst (recordMToEncoding opts targs b),
       Nothing)
      where
          combine = liftA2 (E.><)

instance (Applicative m, G.Selector s, GMToEncoding m arity a) => RecordMToEncoding m arity (G.S1 s a) where
    recordMToEncoding = fieldMToEncoding

instance {-# OVERLAPPING #-} (G.Selector s, MToJSON m a) =>
  RecordMToEncoding m arity (G.S1 s (G.K1 i (Maybe a))) where
    recordMToEncoding opts _ (G.M1 k1) | A.omitNothingFields opts
                                    , G.K1 Nothing <- k1 = (pure E.empty, Nothing)
    recordMToEncoding opts targs m1 = fieldMToEncoding opts targs m1

fieldMToEncoding :: (Applicative m, G.Selector s, GMToEncoding m arity a)
                => A.Options -> MToArgs m A.Encoding arity p
                -> G.S1 s a p -> (m A.Encoding, Maybe (m A.Encoding))
fieldMToEncoding opts targs m1 =
    let keyBuilder = mToEncoding (A.fieldLabelModifier opts $ G.selName m1)
        valueBuilder = gMToEncoding opts targs (G.unM1 m1)
    in  (keyBuilder `combine` (pure E.colon) `combine` valueBuilder, Just valueBuilder)
  where
    combine = liftA2 (E.><)

-- -----------------------------------------------------

class ConsMToEncoding m arity f where
    consMToEncoding :: A.Options -> MToArgs m A.Encoding arity a
                   -> f a -> m A.Encoding

class ConsMToEncoding' m arity f isRecord where
    consMToEncoding' :: A.Options -> MToArgs m A.Encoding arity a
                    -> Bool -- ^ Are we a record with one field?
                    -> f a -> Tagged isRecord (m A.Encoding)

instance ( IsRecord                f isRecord
         , ConsMToEncoding' m arity f isRecord
         ) => ConsMToEncoding m arity f where
    consMToEncoding opts targs =
        (unTagged :: Tagged isRecord (m A.Encoding) -> m A.Encoding)
      . consMToEncoding' opts targs (isUnary (undefined :: f a))

instance (Functor m, RecordMToEncoding m arity f) => ConsMToEncoding' m arity f 'True where
    consMToEncoding' opts targs isUn x =
      let (enc, mbVal) = recordMToEncoding opts targs x
      in case (A.unwrapUnaryRecords opts, isUn, mbVal) of
           (True, True, Just val) -> Tagged val
           _ -> Tagged $ E.wrapObject <$> enc

instance GMToEncoding m arity f => ConsMToEncoding' m arity f 'False where
    consMToEncoding' opts targs _ = Tagged . gMToEncoding opts targs

--------------------------------------------------------------------------------

class MEncodeProduct m arity f where
    mEncodeProduct :: A.Options -> MToArgs m A.Encoding arity a
                  -> f a -> m (E.Encoding' E.InArray)

instance ( Applicative m
         , MEncodeProduct m arity a
         , MEncodeProduct m arity b
         ) => MEncodeProduct m arity (a G.:*: b) where
    mEncodeProduct opts targs (a G.:*: b) | A.omitNothingFields opts =
        fmap (E.econcat . DL.intersperse E.comma .
        filter (not . E.nullEncoding))
        $ sequenceA
        [mEncodeProduct opts targs a, mEncodeProduct opts targs b]
    mEncodeProduct opts targs (a G.:*: b) =
      mEncodeProduct opts targs a `combine`
      mEncodeProduct opts targs b
      where combine = liftA2 (E.>*<)

instance {-# OVERLAPPABLE #-} (Functor m, GMToEncoding m arity a) => MEncodeProduct m arity a where
    mEncodeProduct opts targs a = E.retagEncoding <$> gMToEncoding opts targs a

--------------------------------------------------------------------------------

class TaggedObjectMEnc m arity f where
    taggedObjectMEnc :: A.Options -> MToArgs m A.Encoding arity a
                    -> String -> String
                    -> f a -> m A.Encoding

instance ( TaggedObjectMEnc m arity a
         , TaggedObjectMEnc m arity b
         ) => TaggedObjectMEnc m arity (a G.:+: b) where
    taggedObjectMEnc opts targs tagFieldName contentsFieldName (G.L1 x) =
        taggedObjectMEnc opts targs tagFieldName contentsFieldName x
    taggedObjectMEnc opts targs tagFieldName contentsFieldName (G.R1 x) =
        taggedObjectMEnc opts targs tagFieldName contentsFieldName x

instance ( Applicative m
         , IsRecord a isRecord
         , TaggedObjectMEnc' m arity a isRecord
         , G.Constructor c
         ) => TaggedObjectMEnc m arity (G.C1 c a) where
    taggedObjectMEnc opts targs tagFieldName contentsFieldName v = (E.pairs . E.pair key) <$> val
      where
        key :: T.Text
        key = T.pack tagFieldName
        val = mToEncoding (A.constructorTagModifier opts (G.conName (undefined :: t c a p)))
           `combine` ((unTagged :: Tagged isRecord (m A.Encoding) -> m A.Encoding) . taggedObjectMEnc' opts targs contentsFieldName . G.unM1 $ v)
        combine = liftA2 (E.><)

class TaggedObjectMEnc' m arity f isRecord where
    taggedObjectMEnc' :: A.Options -> MToArgs m A.Encoding arity a
                     -> String -> f a -> Tagged isRecord (m A.Encoding)

instance {-# OVERLAPPABLE #-} Applicative m  => TaggedObjectMEnc' m arity G.U1 'False where
    taggedObjectMEnc' _ _ _ _ = Tagged (pure E.empty)

instance (Functor m, RecordMToEncoding m arity f) => TaggedObjectMEnc' m arity f 'True where
    taggedObjectMEnc' opts targs _ = Tagged . fmap (E.comma E.><) . fst
                                           . recordMToEncoding opts targs

instance (Applicative m, GMToEncoding m arity f) => TaggedObjectMEnc' m arity f 'False where
    taggedObjectMEnc' opts targs contentsFieldName =
        Tagged . (\z -> (pure E.comma) `combine` mToEncoding contentsFieldName `combine` (pure E.colon) `combine` z) .
        gMToEncoding opts targs
      where combine = liftA2 (E.><)

--------------------------------------------------------------------------------

class ObjectWithSingleFieldMEnc m arity f where
    objectWithSingleFieldMEnc :: A.Options -> MToArgs m A.Encoding arity a
                             -> f a -> m A.Encoding

instance ( ObjectWithSingleFieldMEnc m arity a
         , ObjectWithSingleFieldMEnc m arity b
         ) => ObjectWithSingleFieldMEnc m arity (a G.:+: b) where
    objectWithSingleFieldMEnc opts targs (G.L1 x) =
      objectWithSingleFieldMEnc opts targs x
    objectWithSingleFieldMEnc opts targs (G.R1 x) =
      objectWithSingleFieldMEnc opts targs x

instance ( Functor m
         , GMToEncoding m arity a
         , ConsMToEncoding m arity a
         , G.Constructor c
         ) => ObjectWithSingleFieldMEnc m arity (G.C1 c a) where
    objectWithSingleFieldMEnc opts targs v = (E.pairs . E.pair key) <$> val
      where
        key :: T.Text
        key = T.pack (A.constructorTagModifier opts (G.conName (undefined :: t c a p)))
        val :: m (E.Encoding' A.Value)
        val = gMToEncoding opts targs v

--------------------------------------------------------------------------------

class TwoElemArrayMEnc m arity f where
    twoElemArrayMEnc :: A.Options -> MToArgs m A.Encoding arity a
                    -> f a -> m A.Encoding

instance ( TwoElemArrayMEnc m arity a
         , TwoElemArrayMEnc m arity b
         ) => TwoElemArrayMEnc m arity (a G.:+: b) where
    twoElemArrayMEnc opts targs (G.L1 x) = twoElemArrayMEnc opts targs x
    twoElemArrayMEnc opts targs (G.R1 x) = twoElemArrayMEnc opts targs x

instance ( Applicative m
         , GMToEncoding m arity a
         , ConsMToEncoding m arity a
         , G.Constructor c
         ) => TwoElemArrayMEnc m arity (G.C1 c a) where
    twoElemArrayMEnc opts targs x = E.list id <$> sequenceA
      [ mToEncoding (A.constructorTagModifier opts (G.conName (undefined :: t c a p)))
      , gMToEncoding opts targs x
      ]

--------------------------------------------------------------------------------

class UntaggedValueMEnc m arity f where
    untaggedValueMEnc :: A.Options -> MToArgs m A.Encoding arity a
                     -> f a -> m A.Encoding

instance
    ( UntaggedValueMEnc m arity a
    , UntaggedValueMEnc m arity b
    ) => UntaggedValueMEnc m arity (a G.:+: b)
  where
    untaggedValueMEnc opts targs (G.L1 x) = untaggedValueMEnc opts targs x
    untaggedValueMEnc opts targs (G.R1 x) = untaggedValueMEnc opts targs x

instance {-# OVERLAPPABLE #-}
    ( GMToEncoding m arity a
    , ConsMToEncoding m arity a
    ) => UntaggedValueMEnc m arity (G.C1 c a)
  where
    untaggedValueMEnc = gMToEncoding

instance {-# OVERLAPPING #-}
    ( Applicative m, G.Constructor c )
    => UntaggedValueMEnc m arity (G.C1 c G.U1)
  where
    untaggedValueMEnc opts _ _ = mToEncoding $
        A.constructorTagModifier opts $ G.conName (undefined :: t c G.U1 p)

-------------------------------------------------------------------------------

class SumMToEncoding m arity f allNullary where
    sumMToEncoding :: A.Options -> MToArgs m A.Encoding arity a
                  -> f a -> Tagged allNullary (m A.Encoding)

instance ( Applicative m
         , GetConName f
         , TaggedObjectMEnc m arity f
         , ObjectWithSingleFieldMEnc m arity f
         , TwoElemArrayMEnc m arity f
         , UntaggedValueMEnc m arity f
         ) => SumMToEncoding m arity f 'True where
    sumMToEncoding opts targs
        | A.allNullaryToStringTag opts = Tagged . mToEncoding .
                                       A.constructorTagModifier opts . getConName
        | otherwise = Tagged . nonAllNullarySumMToEncoding opts targs

instance ( TwoElemArrayMEnc m arity f
         , TaggedObjectMEnc m arity f
         , ObjectWithSingleFieldMEnc m arity f
         , UntaggedValueMEnc m arity f
         ) => SumMToEncoding m arity f 'False where
    sumMToEncoding opts targs = Tagged . nonAllNullarySumMToEncoding opts targs

nonAllNullarySumMToEncoding :: ( TwoElemArrayMEnc m arity f
                              , TaggedObjectMEnc m arity f
                              , ObjectWithSingleFieldMEnc m arity f
                              , UntaggedValueMEnc m arity f
                              ) => A.Options -> MToArgs m A.Encoding arity a
                                -> f a -> m A.Encoding
nonAllNullarySumMToEncoding opts targs =
    case A.sumEncoding opts of
      A.TaggedObject{..}      ->
        taggedObjectMEnc opts targs tagFieldName contentsFieldName
      A.ObjectWithSingleField -> objectWithSingleFieldMEnc opts targs
      A.TwoElemArray          -> twoElemArrayMEnc opts targs
      A.UntaggedValue         -> untaggedValueMEnc opts targs

--------------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} (GMToEncoding m arity a) => GMToEncoding m arity (G.M1 i c a) where
    -- Meta-information, which is not handled elsewhere, is ignored:
    gMToEncoding opts targs = gMToEncoding opts targs . G.unM1

instance (MToJSON m a) => GMToEncoding m arity (G.K1 i a) where
    -- Constant values are encoded using their ToJSON instance:
    gMToEncoding _opts _ = mToEncoding . G.unK1

instance GMToEncoding m A.One G.Par1 where
    -- Direct occurrences of the last type parameter are encoded with the
    -- function passed in as an argument:
    gMToEncoding _opts (MTo1Args te _) = te . G.unPar1

instance (MToJSON1 m f) => GMToEncoding m A.One (G.Rec1 f) where
    -- Recursive occurrences of the last type parameter are encoded using their
    -- ToEncoding1 instance:
    gMToEncoding _opts (MTo1Args te tel) = mLiftToEncoding te tel . G.unRec1

instance Applicative m => GMToEncoding m arity G.U1 where
    -- Empty constructors are encoded to an empty array:
    gMToEncoding _opts _ _ = pure E.emptyArray_

instance (ConsMToEncoding m arity a) => GMToEncoding m arity (G.C1 c a) where
    -- Constructors need to be encoded differently depending on whether they're
    -- a record or not. This distinction is made by 'consToEncoding':
    gMToEncoding opts targs = consMToEncoding opts targs . G.unM1

instance ( Applicative m
         , MEncodeProduct m arity a
         , MEncodeProduct m arity b
         ) => GMToEncoding m arity (a G.:*: b) where
    -- Products are encoded to an array. Here we allocate a mutable vector of
    -- the same size as the product and write the product's elements to it using
    -- 'encodeProduct':
    gMToEncoding opts targs p = (\a -> E.list E.retagEncoding [a]) <$> mEncodeProduct opts targs p

instance ( AllNullary (a G.:+: b) allNullary
         , SumMToEncoding m arity (a G.:+: b) allNullary
         ) => GMToEncoding m arity (a G.:+: b) where
    -- If all constructors of a sum datatype are nullary and the
    -- 'allNullaryToStringTag' option is set they are encoded to
    -- strings.  This distinction is made by 'sumToEncoding':
    gMToEncoding opts targs
        = (unTagged :: Tagged allNullary (m A.Encoding) -> A.Encoding)
        . sumMToEncoding opts targs

-- instance (ToJSON1 f, GToEncoding One g) => GToEncoding One (f :.: g) where
--     -- If an occurrence of the last type parameter is nested inside two
--     -- composed types, it is encoded by using the outermost type's ToJSON1
--     -- instance to generically encode the innermost type:
--     gToEncoding opts targs =
--       let gte = gToEncoding opts targs in
--       liftToEncoding gte (listEncoding gte) . unComp1

-- -----------------------------------------------------
-- Generic MFromJSON instances
-- -----------------------------------------------------

instance {-# OVERLAPPABLE #-} (GMFromJSON m arity a) => GMFromJSON m arity (G.M1 i c a) where
    -- Meta-information, which is not handled elsewhere, is just added to the
    -- parsed value:
    gMParseJSON opts fargs = fmap (fmap G.M1) . gMParseJSON opts fargs

-- instance (MFromJSON m a) => GMFromJSON m arity (G.K1 i a) where
--     -- Constant values are decoded using their MFromJSON instance:
--     gMParseJSON _opts _ = fmap (fmap G.K1) . mParseJSON

-- instance Applicative m => GMFromJSON m A.One G.Par1 where
--     -- Direct occurrences of the last type parameter are decoded with the
--     -- function passed in as an argument:
--     gMParseJSON _opts (From1Args pj _) = fmap (pure . G.Par1) . pj

-- instance (MFromJSON1 m f) => GMFromJSON m A.One (G.Rec1 f) where
--     -- Recursive occurrences of the last type parameter are decoded using their
--     -- FromJSON1 instance:
--     gMParseJSON _opts (MFrom1Args pj pjl) = fmap (fmap G.Rec1) . mLiftParseJSON pj pjl


-- -----------------------------------------------------
-- MToJSON1, MFromJSON1 Instances
-- -----------------------------------------------------

instance Applicative m => MToJSON1 m Maybe where
    mLiftToEncoding t _ (Just a) = t a
    mLiftToEncoding _  _ Nothing  = pure E.null_

instance Applicative m => MFromJSON1 m Maybe where
    mLiftParseJSON _ _ A.Null = pure (pure Nothing)
    mLiftParseJSON p _ a    = fmap Just <$> p a

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

instance Applicative m => MToJSON m Bool where
    mToEncoding = pure . A.toEncoding

instance Applicative m => MFromJSON m Bool where
    mParseJSON = fmap pure . A.parseJSON

instance Applicative m => MToJSON m Char where
    mToEncoding = pure . A.toEncoding

instance Applicative m => MFromJSON m Char where
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

instance (MToJSON m a) => MToJSON m (Maybe a) where
    mToEncoding = mToEncoding1

instance (MFromJSON m a) => MFromJSON m (Maybe a) where
    mParseJSON = mParseJSON1
