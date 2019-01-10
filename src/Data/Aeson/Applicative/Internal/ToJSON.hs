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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Applicative.Internal.ToJSON where

import Control.Applicative
import Control.Applicative (Const(..))
import Data.Aeson
import Data.Aeson.Applicative.Internal.Generic
import qualified Data.Aeson.Encoding as E
import Data.Aeson.Encoding.Internal
    ( Encoding
    , Encoding'(..)
    , InArray
    , Series(..)
    , closeBracket
    , colon
    , comma
    , econcat
    , emptyArray_
    , list
    , nullEncoding
    , openBracket
    , retagEncoding
    , string
    , (>*<)
    , (><)
    )
import Data.Aeson.Types
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Fixed (Fixed, HasResolution)
import Data.Foldable (toList)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(..))
import Data.Functor.Sum (Sum(..))
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HashSet
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Monoid as Monoid
import Data.Proxy (Proxy(..))
import Data.Ratio (Ratio)
import Data.Scientific (Scientific)
import qualified Data.Semigroup as Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Tagged (Tagged(..))
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as LT
import Data.Time
    ( Day
    , DiffTime
    , LocalTime
    , NominalDiffTime
    , TimeOfDay
    , UTCTime
    , ZonedTime
    )
import qualified Data.Tree as Tree
import qualified Data.UUID.Types as UUID
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Data.Version (Version)
import Data.Void (Void, absurd)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types (CTime(..))
import Foreign.Storable (Storable)
import GHC.Generics
import Numeric.Natural (Natural)

#if MIN_VERSION_primitive(0,6,4)
import qualified Data.Primitive.Array as PM
import qualified Data.Primitive.PrimArray as PM
import qualified Data.Primitive.SmallArray as PM
import qualified Data.Primitive.Types as PM
import qualified Data.Primitive.UnliftedArray as PM
import qualified GHC.Exts as Exts
#endif

-- Based on aeson-1.4.2.0

-- #############################################################################
-- Data.Aeson.Types.ToJSON
-- #############################################################################

-------------------------------------------------------------------------------
-- Generics
-------------------------------------------------------------------------------

-- | Class of generic representation types that can be converted to
-- JSON.
class AGToJSON m enc arity f where
    -- | This method (applied to 'defaultOptions') is used as the
    -- default generic implementation of 'toJSON'
    -- (with @enc ~ 'Value'@ and @arity ~ 'Zero'@)
    -- and 'liftToJSON' (if the @arity@ is 'One').
    --
    -- It also provides a generic implementation of 'toEncoding'
    -- (with @enc ~ 'Encoding'@ and @arity ~ 'Zero'@)
    -- and 'liftToEncoding' (if the @arity@ is 'One').
    agToJSON :: Options -> AToArgs m enc arity a -> f a -> m enc

-- | A 'ToArgs' value either stores nothing (for 'ToJSON') or it stores the two
-- function arguments that encode occurrences of the type parameter (for
-- 'ToJSON1').
data AToArgs m res arity a where
    ANoToArgs :: AToArgs m res Zero a
    ATo1Args  :: (a -> m res) -> ([a] -> m res) -> AToArgs m res One a

-- | A configurable generic JSON encoder. This function applied to
-- 'defaultOptions' is used as the default for 'atoEncoding' when the type
-- is an instance of 'Generic'.
agenericToEncoding :: (Generic a, AGToJSON m Encoding Zero (Rep a))
                  => Options -> a -> m Encoding
agenericToEncoding opts = agToJSON opts ANoToArgs . from

-- | A configurable generic JSON encoder. This function applied to
-- 'defaultOptions' is used as the default for 'liftToEncoding' when the type
-- is an instance of 'Generic1'.
agenericLiftToEncoding :: (Generic1 f, AGToJSON m Encoding One (Rep1 f))
                      => Options -> (a -> m Encoding) -> ([a] -> m Encoding)
                      -> f a -> m Encoding
agenericLiftToEncoding opts te tel = agToJSON opts (ATo1Args te tel) . from1

-- -----------------------------------------------------
-- Class
-- -----------------------------------------------------

-- | Analogous to 'toJSON'
class Applicative m => AToJSON m a where
    atoEncoding :: a -> m Encoding
    {-# INLINE atoEncoding #-}

    default atoEncoding :: (Generic a, AGToJSON m Encoding Zero (Rep a)) => a -> m Encoding
    atoEncoding = agenericToEncoding defaultOptions

    atoEncodingList :: [a] -> m Encoding
    atoEncodingList = alistEncoding atoEncoding
    {-# INLINE atoEncodingList #-}

-------------------------------------------------------------------------------
-- Lifings of FromJSON and ToJSON to unary and binary type constructors
-------------------------------------------------------------------------------

-- | Analogous to 'toJSON1'
class Applicative m => AToJSON1 m f where
    aliftToEncoding :: (a -> m Encoding) -> ([a] -> m Encoding) -> f a -> m Encoding

    default aliftToEncoding :: (Generic1 f, AGToJSON m Encoding One (Rep1 f))
                           => (a -> m Encoding) -> ([a] -> m Encoding)
                           -> f a -> m Encoding
    aliftToEncoding = agenericLiftToEncoding defaultOptions

    aliftToEncodingList :: (a -> m Encoding) -> ([a] -> m Encoding) -> [f a] -> m Encoding
    aliftToEncodingList f g = alistEncoding (aliftToEncoding f g)

-- | Lift the standard 'toEncoding' function through the type constructor.
atoEncoding1 :: (AToJSON1 m f, AToJSON m a) => f a -> m Encoding
atoEncoding1 = aliftToEncoding atoEncoding atoEncodingList
{-# INLINE atoEncoding1 #-}

-- | Lifting of the 'ToJSON' class to binary type constructors.
--
-- Instead of manually writing your 'ToJSON2' instance, "Data.Aeson.TH"
-- provides Template Haskell functions which will derive an instance at compile time.
--
-- The compiler cannot provide a default generic implementation for 'liftToJSON2',
-- unlike 'toJSON' and 'liftToJSON'.
class Applicative m => AToJSON2 m f where
    aliftToEncoding2 :: (a -> m Encoding) -> ([a] -> m Encoding) -> (b -> m Encoding) -> ([b] -> m Encoding) -> f a b -> m Encoding
    aliftToEncodingList2 :: (a -> m Encoding) -> ([a] -> m Encoding) -> (b -> m Encoding) -> ([b] -> m Encoding) -> [f a b] -> m Encoding
    aliftToEncodingList2 fa ga fb gb = alistEncoding (aliftToEncoding2 fa ga fb gb)

-- | Lift the standard 'toEncoding' function through the type constructor.
atoEncoding2 :: (AToJSON2 m f, AToJSON m a, AToJSON m b) => f a b -> m Encoding
atoEncoding2 = aliftToEncoding2 atoEncoding atoEncodingList atoEncoding atoEncodingList
{-# INLINE atoEncoding2 #-}

-------------------------------------------------------------------------------
-- Encoding functions
-------------------------------------------------------------------------------

-- | Helper function to use with 'aliftToEncoding'.
-- Useful when writing own 'AToJSON1' instances.
alistEncoding :: Applicative m => (a -> m Encoding) -> [a] -> m Encoding
alistEncoding _  [] = pure emptyArray_
alistEncoding f (x:xs) = (pure openBracket) `combine` f x `combine` commas xs `combine` (pure closeBracket)
  where
    combine = liftA2 (><)
    commas = foldr (\v vs -> (pure comma) `combine` f v `combine` vs) (pure $ Encoding mempty)
{-# INLINE alistEncoding #-}

-- | Encode as JSON object
-- Based on Data.Aeson.Encoding.Internal.dict
adictEncoding
    :: Applicative m
    => (k -> Encoding' Text)                     -- ^ key encoding
    -> (v -> m Encoding)                         -- ^ value encoding
    -> (forall a. (k -> v -> a -> a) -> a -> c -> a)  -- ^ @foldrWithKey@ - indexed fold
    -> c                                              -- ^ container
    -> m Encoding
adictEncoding encodeKey encodeVal foldrWithKey = foldrWithKey go (pure $ Encoding mempty)
  where
    combine = liftA2 (><)
    go k v c = encodeKV k v `combine` (pure comma) `combine` c
    encodeKV k v = (pure . retagEncoding $ encodeKey k) `combine` (pure colon) `combine` (retagEncoding <$> encodeVal v)
{-# INLINE adictEncoding #-}

-------------------------------------------------------------------------------
-- [] instances
-------------------------------------------------------------------------------

instance Applicative m => AToJSON1 m [] where
    aliftToEncoding _ p' = p'
    {-# INLINE aliftToEncoding #-}

instance AToJSON m a => AToJSON m [a] where
    {-# SPECIALIZE instance Applicative m => AToJSON m String #-}
    {-# SPECIALIZE instance Applicative m => AToJSON m [String] #-}
    {-# SPECIALIZE instance Applicative m => AToJSON m [Array] #-}
    {-# SPECIALIZE instance Applicative m => AToJSON m [Object] #-}

    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}

-------------------------------------------------------------------------------
-- Generic toJSON / toEncoding
-------------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} (AGToJSON m enc arity a) => AGToJSON m enc arity (M1 i c a) where
    -- Meta-information, which is not handled elsewhere, is ignored:
    agToJSON opts targs = agToJSON opts targs . unM1
    {-# INLINE agToJSON #-}

instance AGToJSON m enc One Par1 where
    -- Direct occurrences of the last type parameter are encoded with the
    -- function passed in as an argument:
    agToJSON _opts (ATo1Args tj _) = tj . unPar1
    {-# INLINE agToJSON #-}

instance ( AConsToJSON m enc arity a
         , AllNullary (C1 c a) allNullary
         , ASumToJSON m enc arity (C1 c a) allNullary
         ) => AGToJSON m enc arity (D1 d (C1 c a)) where
    -- The option 'tagSingleConstructors' determines whether to wrap
    -- a single-constructor type.
    agToJSON opts targs
#if MIN_VERSION_aeson(1,3,0)
        | tagSingleConstructors opts = (unTagged :: Tagged allNullary (m enc) -> m enc)
                                     . asumToJSON opts targs
                                     . unM1
#endif
        | otherwise = aconsToJSON opts targs . unM1 . unM1
    {-# INLINE agToJSON #-}

instance (AConsToJSON m enc arity a) => AGToJSON m enc arity (C1 c a) where
    -- Constructors need to be encoded differently depending on whether they're
    -- a record or not. This distinction is made by 'consToJSON':
    agToJSON opts targs = aconsToJSON opts targs . unM1
    {-# INLINE agToJSON #-}

instance ( AllNullary (a :+: b) allNullary
         , ASumToJSON  m enc arity (a :+: b) allNullary
         ) => AGToJSON m enc arity (a :+: b)
  where
    -- If all constructors of a sum datatype are nullary and the
    -- 'allNullaryToStringTag' option is set they are encoded to
    -- strings.  This distinction is made by 'sumToJSON':
    agToJSON opts targs = (unTagged :: Tagged allNullary (m enc) -> m enc)
                       . asumToJSON opts targs
    {-# INLINE agToJSON #-}

--------------------------------------------------------------------------------
-- Generic toEncoding

instance AToJSON m a => AGToJSON m Encoding arity (K1 i a) where
    -- Constant values are encoded using their ToJSON instance:
    agToJSON _opts _ = atoEncoding . unK1
    {-# INLINE agToJSON #-}

instance AToJSON1 m f => AGToJSON m Encoding One (Rec1 f) where
    -- Recursive occurrences of the last type parameter are encoded using their
    -- ToEncoding1 instance:
    agToJSON _opts (ATo1Args te tel) = aliftToEncoding te tel . unRec1
    {-# INLINE agToJSON #-}

instance Applicative m => AGToJSON m Encoding arity U1 where
    -- Empty constructors are encoded to an empty array:
    agToJSON _opts _ _ = pure emptyArray_
    {-# INLINE agToJSON #-}

instance ( Applicative m
         , AEncodeProduct m arity a
         , AEncodeProduct m arity b
         ) => AGToJSON m Encoding arity (a :*: b)
  where
    -- Products are encoded to an array. Here we allocate a mutable vector of
    -- the same size as the product and write the product's elements to it using
    -- 'encodeProduct':
    agToJSON opts targs p = fmap (list retagEncoding) $ sequenceA [aencodeProduct opts targs p]
    {-# INLINE agToJSON #-}

instance ( AToJSON1 m f
         , AGToJSON m Encoding One g
         ) => AGToJSON m Encoding One (f :.: g)
  where
    -- If an occurrence of the last type parameter is nested inside two
    -- composed types, it is encoded by using the outermost type's ToJSON1
    -- instance to generically encode the innermost type:
    agToJSON opts targs =
      let gte = agToJSON opts targs in
      aliftToEncoding gte (alistEncoding gte) . unComp1
    {-# INLINE agToJSON #-}

--------------------------------------------------------------------------------

class ASumToJSON m enc arity f allNullary where
    asumToJSON :: Options -> AToArgs m enc arity a
              -> f a -> Tagged allNullary (m enc)

instance
    ( Applicative m
    , GetConName f
    , FromString enc
    , ATaggedObject m enc arity f
    , ASumToJSON' m ObjectWithSingleField enc arity f
    , ASumToJSON' m TwoElemArray enc arity f
    , ASumToJSON' m UntaggedValue enc arity f
    ) => ASumToJSON m enc arity f 'True
  where
    asumToJSON opts targs
        | allNullaryToStringTag opts = Tagged . pure . fromString
                                     . constructorTagModifier opts . getConName
        | otherwise = Tagged . anonAllNullarySumToJSON opts targs

instance ( ATaggedObject m enc arity f
         , ASumToJSON' m ObjectWithSingleField enc arity f
         , ASumToJSON' m TwoElemArray enc arity f
         , ASumToJSON' m UntaggedValue enc arity f
         ) => ASumToJSON m enc arity f 'False
  where
    asumToJSON opts targs = Tagged . anonAllNullarySumToJSON opts targs

anonAllNullarySumToJSON ::
    ( ATaggedObject m enc arity f
    , ASumToJSON' m ObjectWithSingleField enc arity f
    , ASumToJSON' m TwoElemArray enc arity f
    , ASumToJSON' m UntaggedValue enc arity f
    ) => Options -> AToArgs m enc arity a
    -> f a -> m enc
anonAllNullarySumToJSON opts targs =
    case sumEncoding opts of

      TaggedObject{..}      ->
        ataggedObject opts targs tagFieldName contentsFieldName

      ObjectWithSingleField ->
        (unTagged :: Tagged ObjectWithSingleField (m enc) -> m enc)
          . asumToJSON' opts targs

      TwoElemArray          ->
        (unTagged :: Tagged TwoElemArray (m enc) -> m enc)
          . asumToJSON' opts targs

      UntaggedValue         ->
        (unTagged :: Tagged UntaggedValue (m enc) -> m enc)
          . asumToJSON' opts targs

--------------------------------------------------------------------------------

class FromString enc where
  fromString :: String -> enc

instance FromString Encoding where
  fromString = toEncoding

instance FromString Value where
  fromString = String . pack

--------------------------------------------------------------------------------

class ATaggedObject m enc arity f where
    ataggedObject :: Options -> AToArgs m enc arity a
                 -> String -> String
                 -> f a -> m enc

instance ( ATaggedObject m enc arity a
         , ATaggedObject m enc arity b
         ) => ATaggedObject m enc arity (a :+: b)
  where
    ataggedObject opts targs tagFieldName contentsFieldName (L1 x) =
        ataggedObject opts targs tagFieldName contentsFieldName x
    ataggedObject opts targs tagFieldName contentsFieldName (R1 x) =
        ataggedObject opts targs tagFieldName contentsFieldName x

instance
    ( Functor m
    , IsRecord a isRecord
    , ATaggedObject' m enc pairs arity a isRecord
    , FromPairs enc pairs
    , FromString enc
    , KeyValuePair enc pairs
    , Constructor c
    ) => ATaggedObject m enc arity (C1 c a)
  where
    ataggedObject opts targs tagFieldName contentsFieldName =
      fmap (fromPairs . mappend tag) . contents
      where
        tag = tagFieldName `pair`
          (fromString (constructorTagModifier opts (conName (undefined :: t c a p)))
            :: enc)
        contents =
          (unTagged :: Tagged isRecord (m pairs) -> m pairs) .
            ataggedObject' opts targs contentsFieldName . unM1

class ATaggedObject' m enc pairs arity f isRecord where
    ataggedObject' :: Options -> AToArgs m enc arity a
                  -> String -> f a -> Tagged isRecord (m pairs)

instance
    ( Functor m
    , AGToJSON m enc arity f
    , KeyValuePair enc pairs
    ) => ATaggedObject' m enc pairs arity f 'False
  where
    ataggedObject' opts targs contentsFieldName =
        Tagged . fmap (contentsFieldName `pair`) . agToJSON opts targs

instance {-# OVERLAPPING #-} (Applicative m, Monoid pairs) => ATaggedObject' m enc pairs arity U1 'False where
    ataggedObject' _ _ _ _ = Tagged (pure mempty)

instance ( ARecordToPairs m enc pairs arity f
         ) => ATaggedObject' m enc pairs arity f 'True
  where
    ataggedObject' opts targs _ = Tagged . arecordToPairs opts targs

--------------------------------------------------------------------------------

-- | Get the name of the constructor of a sum datatype.
-- Not exported from Data.Aeson.Types.ToJSON
class GetConName f where
    getConName :: f a -> String

instance (GetConName a, GetConName b) => GetConName (a :+: b) where
    getConName (L1 x) = getConName x
    getConName (R1 x) = getConName x

instance (Constructor c) => GetConName (C1 c a) where
    getConName = conName

--------------------------------------------------------------------------------

-- Reflection of SumEncoding variants

data ObjectWithSingleField
data TwoElemArray
data UntaggedValue

--------------------------------------------------------------------------------

class ASumToJSON' m s enc arity f where
    asumToJSON' :: Options -> AToArgs m enc arity a
                    -> f a -> Tagged s (m enc)

instance ( ASumToJSON' m s enc arity a
         , ASumToJSON' m s enc arity b
         ) => ASumToJSON' m s enc arity (a :+: b)
  where
    asumToJSON' opts targs (L1 x) = asumToJSON' opts targs x
    asumToJSON' opts targs (R1 x) = asumToJSON' opts targs x

--------------------------------------------------------------------------------

instance
    ( Applicative m
    , AGToJSON m Encoding arity a
    , AConsToJSON m Encoding arity a
    , Constructor c
    ) => ASumToJSON' m TwoElemArray Encoding arity (C1 c a)
  where
    asumToJSON' opts targs x = Tagged $ (list id) <$> sequenceA
      [ atoEncoding (constructorTagModifier opts (conName (undefined :: t c a p)))
      , agToJSON opts targs x
      ]

--------------------------------------------------------------------------------

class AConsToJSON m enc arity f where
    aconsToJSON :: Options -> AToArgs m enc arity a
               -> f a -> m enc

class AConsToJSON' m enc arity f isRecord where
    aconsToJSON' :: Options -> AToArgs m enc arity a
                    -> f a -> Tagged isRecord (m enc)

instance ( IsRecord f isRecord
         , AConsToJSON' m enc arity f isRecord
         ) => AConsToJSON m enc arity f
  where
    aconsToJSON opts targs =
        (unTagged :: Tagged isRecord (m enc) -> m enc)
      . aconsToJSON' opts targs
    {-# INLINE aconsToJSON #-}

instance {-# OVERLAPPING #-}
         ( Functor m
         , ARecordToPairs m enc pairs arity (S1 s f)
         , FromPairs enc pairs
         , AGToJSON m enc arity f
         ) => AConsToJSON' m enc arity (S1 s f) 'True
  where
    aconsToJSON' opts targs
      | unwrapUnaryRecords opts = Tagged . agToJSON opts targs
      | otherwise = Tagged . fmap fromPairs . arecordToPairs opts targs
    {-# INLINE aconsToJSON' #-}

instance
    ( Functor m
    , ARecordToPairs m enc pairs arity f
    , FromPairs enc pairs
    ) => AConsToJSON' m enc arity f 'True
  where
    aconsToJSON' opts targs = Tagged . fmap fromPairs . arecordToPairs opts targs
    {-# INLINE aconsToJSON' #-}

instance AGToJSON m enc arity f => AConsToJSON' m enc arity f 'False where
    aconsToJSON' opts targs = Tagged . agToJSON opts targs
    {-# INLINE aconsToJSON' #-}

--------------------------------------------------------------------------------

class ARecordToPairs m enc pairs arity f where
    -- 1st element: whole thing
    -- 2nd element: in case the record has only 1 field, just the value
    --              of the field (without the key); 'Nothing' otherwise
    arecordToPairs :: Options -> AToArgs m enc arity a
                  -> f a -> m pairs

instance ( Applicative m
         , Monoid pairs
         , ARecordToPairs m enc pairs arity a
         , ARecordToPairs m enc pairs arity b
         ) => ARecordToPairs m enc pairs arity (a :*: b)
  where
    arecordToPairs opts (targs :: AToArgs m enc arity p) (a :*: b) =
        mappend <$> pairsOf a <*> pairsOf b
      where
        pairsOf :: (ARecordToPairs m enc pairs arity f) => f p -> m pairs
        pairsOf = arecordToPairs opts targs
    {-# INLINE arecordToPairs #-}

instance
    ( Applicative m
    , Selector s
    , AGToJSON m enc arity a
    , KeyValuePair enc pairs
    ) => ARecordToPairs m enc pairs arity (S1 s a)
  where
    arecordToPairs = afieldToPair
    {-# INLINE arecordToPairs #-}

instance {-# INCOHERENT #-}
    ( Applicative m
    , Selector s
    , AGToJSON m enc arity (K1 i (Maybe a))
    , KeyValuePair enc pairs
    , Monoid pairs
    ) => ARecordToPairs m enc pairs arity (S1 s (K1 i (Maybe a)))
  where
    arecordToPairs opts _ (M1 k1) | omitNothingFields opts
                                 , K1 Nothing <- k1 = pure mempty
    arecordToPairs opts targs m1 = afieldToPair opts targs m1
    {-# INLINE arecordToPairs #-}

instance {-# INCOHERENT #-}
    ( Applicative m
    , Selector s
    , AGToJSON m enc arity (K1 i (Maybe a))
    , KeyValuePair enc pairs
    , Monoid pairs
    ) => ARecordToPairs m enc pairs arity (S1 s (K1 i (Semigroup.Option a)))
  where
    arecordToPairs opts targs = arecordToPairs opts targs . unwrap
      where
        unwrap :: S1 s (K1 i (Semigroup.Option a)) p -> S1 s (K1 i (Maybe a)) p
        unwrap (M1 (K1 (Semigroup.Option a))) = M1 (K1 a)
    {-# INLINE arecordToPairs #-}

afieldToPair :: ( Applicative m
               , Selector s
               , AGToJSON m enc arity a
               , KeyValuePair enc pairs)
            => Options -> AToArgs m enc arity p
            -> S1 s a p -> m pairs
afieldToPair opts targs m1 =
  let key   = fieldLabelModifier opts (selName m1)
      value = agToJSON opts targs (unM1 m1)
  in pair <$> (pure key) <*> value
{-# INLINE afieldToPair #-}

--------------------------------------------------------------------------------

class AEncodeProduct m arity f where
    aencodeProduct :: Options -> AToArgs m Encoding arity a
                  -> f a -> m (Encoding' InArray)

instance
    ( Applicative m
    , AEncodeProduct m arity a
    , AEncodeProduct m arity b
    ) => AEncodeProduct m arity (a :*: b) where
    aencodeProduct opts targs (a :*: b) | omitNothingFields opts =
        fmap (econcat . intersperse comma .
            filter (not . nullEncoding))
        $ sequenceA [aencodeProduct opts targs a, aencodeProduct opts targs b]
    aencodeProduct opts targs (a :*: b) = (>*<) <$>
      aencodeProduct opts targs a <*>
      aencodeProduct opts targs b
    {-# INLINE aencodeProduct #-}

instance {-# OVERLAPPABLE #-} (Functor m, AGToJSON m Encoding arity a) => AEncodeProduct m arity a where
    aencodeProduct opts targs a = retagEncoding <$> agToJSON opts targs a
    {-# INLINE aencodeProduct #-}

------------------------------------------------------------------------------

instance ( Functor m
         , AGToJSON m enc arity a
         , AConsToJSON m enc arity a
         , FromPairs enc pairs
         , KeyValuePair enc pairs
         , Constructor c
         ) => ASumToJSON' m ObjectWithSingleField enc arity (C1 c a)
  where
    asumToJSON' opts targs =
      Tagged . fmap (fromPairs . (typ `pair`)) . agToJSON opts targs
        where
          typ = constructorTagModifier opts $
                         conName (undefined :: t c a p)

--------------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-}
    ( AConsToJSON m enc arity a
    ) => ASumToJSON' m UntaggedValue enc arity (C1 c a)
  where
    asumToJSON' opts targs = Tagged . agToJSON opts targs

instance {-# OVERLAPPING #-}
    ( Applicative m
    , Constructor c
    , FromString enc
    ) => ASumToJSON' m UntaggedValue enc arity (C1 c U1)
  where
    asumToJSON' opts _ _ = Tagged . pure . fromString $
        constructorTagModifier opts $ conName (undefined :: t c U1 p)

--------------------------------------------------------------------------------
-- Copy of aeson-1.4.2.0, which wasn't availabl in aeson-1.1.2.0
--------------------------------------------------------------------------------
#if !MIN_VERSION_aeson(1,3,0)
-- | Wrap a list of pairs as an object.
class Monoid pairs => FromPairs enc pairs | enc -> pairs where
  fromPairs :: pairs -> enc

instance (a ~ Value) => FromPairs (Encoding' a) Series where
  fromPairs = pairs

instance FromPairs Value (DList Pair) where
  fromPairs = object . DList.toList

-- | Like 'KeyValue' but the value is already converted to JSON
-- ('Value' or 'Encoding'), and the result actually represents lists of pairs
-- so it can be readily concatenated.
class Monoid kv => KeyValuePair v kv where
    pair :: String -> v -> kv

instance (v ~ Value) => KeyValuePair v (DList Pair) where
    pair k v = DList.singleton (pack k .= v)

instance (e ~ Encoding) => KeyValuePair e Series where
    pair = pairStr
      where
        pairStr :: String -> Encoding -> Series
        pairStr name val = pair' (string name) val
        {-# INLINE pairStr #-}

        pair' :: Encoding' Text -> Encoding -> Series
        pair' name val = Value $ retagEncoding $ retagEncoding name >< colon >< val

#endif


-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- base
-------------------------------------------------------------------------------

instance Applicative m => AToJSON2 m Const where
    aliftToEncoding2 t _ _ _ (Const x) = t x
    {-# INLINE aliftToEncoding2 #-}

instance AToJSON m a => AToJSON1 m (Const a) where
    aliftToEncoding _ _ (Const x) = atoEncoding x
    {-# INLINE aliftToEncoding #-}

instance AToJSON m a => AToJSON m (Const a b) where
    atoEncoding (Const x) = atoEncoding x
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON1 m Maybe where
    aliftToEncoding t _ (Just a) = t a
    aliftToEncoding _  _ Nothing  = pure E.null_
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m a) => AToJSON m (Maybe a) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON2 m Either where
    aliftToEncoding2  toA _ _toB _ (Left a) = (E.pairs . E.pair "Left") <$> toA a

    aliftToEncoding2 _toA _ toB _ (Right b) = (E.pairs . E.pair "Right") <$> toB b
    {-# INLINE aliftToEncoding2 #-}

instance (AToJSON m a) => AToJSON1 m (Either a) where
    aliftToEncoding = aliftToEncoding2 atoEncoding atoEncodingList
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m a, AToJSON m b) => AToJSON m (Either a b) where
    atoEncoding = atoEncoding2
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON m Void where
    atoEncoding = absurd
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON m Bool where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON m Ordering where
    atoEncoding = pure . toEncoding

instance Applicative m => AToJSON m () where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}

instance Applicative m => AToJSON m Char where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}

    atoEncodingList = pure . toEncodingList
    {-# INLINE atoEncodingList #-}


instance Applicative m => AToJSON m Double where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}


-- instance Applicative m => AToJSON m Number where
--     atoEncoding = pure . toEncoding
--     {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON m Float where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}


instance (Applicative m, ToJSON a, Integral a) => AToJSON m (Ratio a) where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}


instance (Applicative m, HasResolution a) => AToJSON m (Fixed a) where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON m Int where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON m Integer where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON m Natural where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON m Int8 where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON m Int16 where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON m Int32 where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON m Int64 where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON m Word where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON m Word8 where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON m Word16 where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON m Word32 where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}

instance Applicative m => AToJSON m Word64 where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON m CTime where
    atoEncoding (CTime i) = atoEncoding i
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON m Text where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON m LT.Text where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON m Version where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}

-------------------------------------------------------------------------------
-- semigroups NonEmpty
-------------------------------------------------------------------------------

instance Applicative m => AToJSON1 m NonEmpty where
    aliftToEncoding t _ = alistEncoding t . NE.toList
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m a) => AToJSON m (NonEmpty a) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}

-------------------------------------------------------------------------------
-- scientific
-------------------------------------------------------------------------------

instance Applicative m => AToJSON m Scientific where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}

-------------------------------------------------------------------------------
-- DList
-------------------------------------------------------------------------------

instance Applicative m => AToJSON1 m DList.DList where
    aliftToEncoding t _ = alistEncoding t . toList
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m a) => AToJSON m (DList.DList a) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}

-------------------------------------------------------------------------------
-- transformers - Functors
-------------------------------------------------------------------------------

instance Applicative m => AToJSON1 m Identity where
    aliftToEncoding t _ (Identity a) = t a
    {-# INLINE aliftToEncoding #-}

    aliftToEncodingList _ tl xs = tl (map runIdentity xs)
    {-# INLINE aliftToEncodingList #-}

instance (AToJSON m a) => AToJSON m (Identity a) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}

    atoEncodingList = aliftToEncodingList atoEncoding atoEncodingList
    {-# INLINE atoEncodingList #-}


instance (Applicative m, AToJSON1 m f, AToJSON1 m g) => AToJSON1 m (Compose f g) where
    aliftToEncoding te tel (Compose x) = aliftToEncoding g gl x
      where
        g = aliftToEncoding te tel
        gl = aliftToEncodingList te tel
    {-# INLINE aliftToEncoding #-}

    aliftToEncodingList te tel xs = aliftToEncodingList g gl (map getCompose xs)
      where
        g = aliftToEncoding te tel
        gl = aliftToEncodingList te tel
    {-# INLINE aliftToEncodingList #-}

instance (AToJSON1 m f, AToJSON1 m g, AToJSON m a) => AToJSON m (Compose f g a) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}

    atoEncodingList = aliftToEncodingList atoEncoding atoEncodingList
    {-# INLINE atoEncodingList #-}


instance (AToJSON1 m f, AToJSON1 m g) => AToJSON1 m (Product f g) where
    aliftToEncoding te tel (Pair x y) = aliftToEncoding2 tx txl ty tyl (x, y)
      where
        tx = aliftToEncoding te tel
        txl = aliftToEncodingList te tel
        ty = aliftToEncoding te tel
        tyl = aliftToEncodingList te tel

instance (AToJSON1 m f, AToJSON1 m g, AToJSON m a) => AToJSON m (Product f g a) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}

instance (AToJSON1 m f, AToJSON1 m g) => AToJSON1 m (Sum f g) where
    aliftToEncoding te tel (InL x) = (E.pairs . E.pair "InL") <$> aliftToEncoding te tel x
    aliftToEncoding te tel (InR y) = (E.pairs . E.pair "InR") <$> aliftToEncoding te tel y

instance (AToJSON1 m f, AToJSON1 m g, AToJSON m a) => AToJSON m (Sum f g a) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------

instance Applicative m => AToJSON1 m Seq.Seq where
    aliftToEncoding t _ = alistEncoding t . toList
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m a) => AToJSON m (Seq.Seq a) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON1 m Set.Set where
    aliftToEncoding t _ = alistEncoding t . Set.toList
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m a) => AToJSON m (Set.Set a) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON m IntSet.IntSet where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON1 m IntMap.IntMap where
    aliftToEncoding t tol = aliftToEncoding to' tol' . IntMap.toList
      where
        to'  = aliftToEncoding2     atoEncoding atoEncodingList t tol
        tol' = aliftToEncodingList2 atoEncoding atoEncodingList t tol
    {-# INLINE aliftToEncoding #-}

instance AToJSON m a => AToJSON m (IntMap.IntMap a) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}

instance (Applicative m, ToJSONKey k) => AToJSON1 m (M.Map k) where
    aliftToEncoding g _ = case toJSONKey of
        ToJSONKeyText _ f -> adictEncoding f g M.foldrWithKey
        ToJSONKeyValue _ f -> alistEncoding (pairEncoding f) . M.toList
      where
        pairEncoding f (a, b) = alistEncoding id [pure $ f a, g b]
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m v, ToJSONKey k) => AToJSON m (M.Map k v) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON1 m Tree.Tree where
    aliftToEncoding t tol = go
      where
        go (Tree.Node root branches) =
            aliftToEncoding2 t tol to' tol' (root, branches)

        to' = aliftToEncoding go (alistEncoding go)
        tol' = aliftToEncodingList go (alistEncoding go)
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m v) => AToJSON m (Tree.Tree v) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}

-------------------------------------------------------------------------------
-- uuid
-------------------------------------------------------------------------------

instance Applicative m => AToJSON m UUID.UUID where
    atoEncoding = pure . toEncoding

-------------------------------------------------------------------------------
-- vector
-------------------------------------------------------------------------------

instance {-# OVERLAPPING #-} Applicative m => AToJSON m Array where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}

instance Applicative m => AToJSON1 m Vector where
    aliftToEncoding t _ =  alistEncoding t . V.toList
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m a) => AToJSON m (Vector a) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}

mencodeVector :: (AToJSON m a, VG.Vector v a) => v a -> m Encoding
mencodeVector = alistEncoding atoEncoding . VG.toList
{-# INLINE mencodeVector #-}

instance (Storable a, AToJSON m a) => AToJSON m (VS.Vector a) where
    atoEncoding = mencodeVector
    {-# INLINE atoEncoding #-}


instance (VP.Prim a, AToJSON m a) => AToJSON m (VP.Vector a) where
    atoEncoding = mencodeVector
    {-# INLINE atoEncoding #-}


instance (VG.Vector VU.Vector a, AToJSON m a) => AToJSON m (VU.Vector a) where
    atoEncoding = mencodeVector
    {-# INLINE atoEncoding #-}

-------------------------------------------------------------------------------
-- unordered-containers
-------------------------------------------------------------------------------
instance {-# OVERLAPPING #-} Applicative m => AToJSON m Object where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}

instance Applicative m => AToJSON1 m HashSet.HashSet where
    aliftToEncoding t _ = alistEncoding t . HashSet.toList
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m a) => AToJSON m (HashSet.HashSet a) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}

instance (Applicative m, ToJSONKey k) => AToJSON1 m (H.HashMap k) where
    aliftToEncoding g _ = case toJSONKey of
        ToJSONKeyText _ f -> adictEncoding f g H.foldrWithKey
        ToJSONKeyValue _ f -> alistEncoding (pairEncoding f) . H.toList
      where
        pairEncoding f (a, b) = alistEncoding id [pure $ f a, g b]
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m v, ToJSONKey k) => AToJSON m (H.HashMap k v) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

instance Applicative m => AToJSON m Value where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}

instance Applicative m => AToJSON m DotNetTime where
    atoEncoding = pure . toEncoding

-------------------------------------------------------------------------------
-- primitive
-------------------------------------------------------------------------------

#if (MIN_VERSION_primitive(0,6,4))
instance (AToJSON m a) => AToJSON m (PM.Array a) where
  -- note: we could do better than this if vector exposed the data
  -- constructor in Data.Vector.
  atoEncoding = atoEncoding . Exts.toList

instance (AToJSON m a) => AToJSON m (PM.SmallArray a) where
  atoEncoding = atoEncoding . Exts.toList

instance (PM.Prim a, AToJSON m a) => AToJSON m (PM.PrimArray a) where
  atoEncoding = atoEncoding . Exts.toList

instance (PM.PrimUnlifted a, AToJSON m a) => AToJSON m (PM.UnliftedArray a) where
  atoEncoding = atoEncoding . Exts.toList
#endif

-------------------------------------------------------------------------------
-- time
-------------------------------------------------------------------------------

instance Applicative m => AToJSON m Day where
    atoEncoding = pure . toEncoding

instance Applicative m => AToJSON m TimeOfDay where
    atoEncoding = pure . toEncoding


instance Applicative m => AToJSON m LocalTime where
    atoEncoding = pure . toEncoding


instance Applicative m => AToJSON m ZonedTime where
    atoEncoding = pure . toEncoding


instance Applicative m => AToJSON m UTCTime where
    atoEncoding = pure . toEncoding

instance Applicative m => AToJSON m NominalDiffTime where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON m DiffTime where
    atoEncoding = pure . E.scientific . realToFrac
    {-# INLINE atoEncoding #-}

-------------------------------------------------------------------------------
-- base Monoid/Semigroup
-------------------------------------------------------------------------------

instance Applicative m => AToJSON1 m Monoid.Dual where
    aliftToEncoding t _ = t . Monoid.getDual
    {-# INLINE aliftToEncoding #-}

instance AToJSON m a => AToJSON m (Monoid.Dual a) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON1 m Monoid.First where
    aliftToEncoding t to' = aliftToEncoding t to' . Monoid.getFirst
    {-# INLINE aliftToEncoding #-}

instance AToJSON m a => AToJSON m (Monoid.First a) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON1 m Monoid.Last where
    aliftToEncoding t to' = aliftToEncoding t to' . Monoid.getLast
    {-# INLINE aliftToEncoding #-}

instance AToJSON m a => AToJSON m (Monoid.Last a) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON1 m Semigroup.Min where
    aliftToEncoding t _ (Semigroup.Min x) = t x
    {-# INLINE aliftToEncoding #-}

instance AToJSON m a => AToJSON m (Semigroup.Min a) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON1 m Semigroup.Max where
    aliftToEncoding t _ (Semigroup.Max x) = t x
    {-# INLINE aliftToEncoding #-}

instance AToJSON m a => AToJSON m (Semigroup.Max a) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}

instance Applicative m => AToJSON1 m Semigroup.First where
    aliftToEncoding t _ (Semigroup.First x) = t x
    {-# INLINE aliftToEncoding #-}

instance AToJSON m a => AToJSON m (Semigroup.First a) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON1 m Semigroup.Last where
    aliftToEncoding t _ (Semigroup.Last x) = t x
    {-# INLINE aliftToEncoding #-}

instance AToJSON m a => AToJSON m (Semigroup.Last a) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON1 m Semigroup.WrappedMonoid where
    aliftToEncoding t _ (Semigroup.WrapMonoid x) = t x
    {-# INLINE aliftToEncoding #-}

instance AToJSON m a => AToJSON m (Semigroup.WrappedMonoid a) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON1 m Semigroup.Option where
    aliftToEncoding t to' = aliftToEncoding t to' . Semigroup.getOption
    {-# INLINE aliftToEncoding #-}

instance AToJSON m a => AToJSON m (Semigroup.Option a) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}

-------------------------------------------------------------------------------
-- tagged
-------------------------------------------------------------------------------

instance Applicative m => AToJSON1 m Proxy where
    aliftToEncoding _ _ _ = pure E.null_
    {-# INLINE aliftToEncoding #-}

instance Applicative m => AToJSON m (Proxy a) where
    atoEncoding = pure . toEncoding
    {-# INLINE atoEncoding #-}


instance Applicative m => AToJSON2 m Tagged where
    aliftToEncoding2 _ _ t _ (Tagged x) = t x
    {-# INLINE aliftToEncoding2 #-}

instance Applicative m => AToJSON1 m (Tagged a) where
    aliftToEncoding t _ (Tagged x) = t x
    {-# INLINE aliftToEncoding #-}

instance AToJSON m b => AToJSON m (Tagged a b) where
    atoEncoding = atoEncoding1
    {-# INLINE atoEncoding #-}


-------------------------------------------------------------------------------
-- Tuple instances
-------------------------------------------------------------------------------

instance Applicative m => AToJSON2 m (,) where
    aliftToEncoding2 toA _ toB _ (a, b) = E.list id <$> sequenceA [toA a, toB b]
    {-# INLINE aliftToEncoding2 #-}

instance (AToJSON m a) => AToJSON1 m ((,) a) where
    aliftToEncoding = aliftToEncoding2 atoEncoding atoEncodingList
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m a, AToJSON m b) => AToJSON m (a, b) where
    atoEncoding = atoEncoding2
    {-# INLINE atoEncoding #-}

instance (AToJSON m a) => AToJSON2 m ((,,) a) where
    aliftToEncoding2 toB _ toC _ (a, b, c) = E.list id <$> sequenceA
      [ atoEncoding a
      , toB b
      , toC c
      ]
    {-# INLINE aliftToEncoding2 #-}

instance (AToJSON m a, AToJSON m b) => AToJSON1 m ((,,) a b) where
    aliftToEncoding = aliftToEncoding2 atoEncoding atoEncodingList
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c) => AToJSON m (a, b, c) where
    atoEncoding = atoEncoding2
    {-# INLINE atoEncoding #-}

instance (AToJSON m a, AToJSON m b) => AToJSON2 m ((,,,) a b) where
    aliftToEncoding2 toC _ toD _ (a, b, c, d) = E.list id <$> sequenceA
      [ atoEncoding a
      , atoEncoding b
      , toC c
      , toD d
      ]
    {-# INLINE aliftToEncoding2 #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c) => AToJSON1 m ((,,,) a b c) where
    aliftToEncoding = aliftToEncoding2 atoEncoding atoEncodingList
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d) => AToJSON m (a, b, c, d) where
    atoEncoding = atoEncoding2
    {-# INLINE atoEncoding #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c) => AToJSON2 m ((,,,,) a b c) where
    aliftToEncoding2 toD _ toE _ (a, b, c, d, e) = E.list id <$> sequenceA
      [ atoEncoding a
      , atoEncoding b
      , atoEncoding c
      , toD d
      , toE e
      ]
    {-# INLINE aliftToEncoding2 #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d) => AToJSON1 m ((,,,,) a b c d) where
    aliftToEncoding = aliftToEncoding2 atoEncoding atoEncodingList
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e) => AToJSON m (a, b, c, d, e) where
    atoEncoding = atoEncoding2
    {-# INLINE atoEncoding #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d) => AToJSON2 m ((,,,,,) a b c d) where
    aliftToEncoding2 toE _ toF _ (a, b, c, d, e, f) = E.list id <$> sequenceA
      [ atoEncoding a
      , atoEncoding b
      , atoEncoding c
      , atoEncoding d
      , toE e
      , toF f
      ]
    {-# INLINE aliftToEncoding2 #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e) => AToJSON1 m ((,,,,,) a b c d e) where
    aliftToEncoding = aliftToEncoding2 atoEncoding atoEncodingList
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f) => AToJSON m (a, b, c, d, e, f) where
    atoEncoding = atoEncoding2
    {-# INLINE atoEncoding #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e) => AToJSON2 m ((,,,,,,) a b c d e) where
    aliftToEncoding2 toF _ toG _ (a, b, c, d, e, f, g) = E.list id <$> sequenceA
        [ atoEncoding a
        , atoEncoding b
        , atoEncoding c
        , atoEncoding d
        , atoEncoding e
        , toF f
        , toG g
        ]
    {-# INLINE aliftToEncoding2 #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f) => AToJSON1 m ((,,,,,,) a b c d e f) where
    aliftToEncoding = aliftToEncoding2 atoEncoding atoEncodingList
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f, AToJSON m g) => AToJSON m (a, b, c, d, e, f, g) where
    atoEncoding = atoEncoding2
    {-# INLINE atoEncoding #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f) => AToJSON2 m ((,,,,,,,) a b c d e f) where
    aliftToEncoding2 toG _ toH _ (a, b, c, d, e, f, g, h) = E.list id <$> sequenceA
        [ atoEncoding a
        , atoEncoding b
        , atoEncoding c
        , atoEncoding d
        , atoEncoding e
        , atoEncoding f
        , toG g
        , toH h
        ]
    {-# INLINE aliftToEncoding2 #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f, AToJSON m g) => AToJSON1 m ((,,,,,,,) a b c d e f g) where
    aliftToEncoding = aliftToEncoding2 atoEncoding atoEncodingList
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f, AToJSON m g, AToJSON m h) => AToJSON m (a, b, c, d, e, f, g, h) where
    atoEncoding = atoEncoding2
    {-# INLINE atoEncoding #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f, AToJSON m g) => AToJSON2 m ((,,,,,,,,) a b c d e f g) where
    aliftToEncoding2 toH _ toI _ (a, b, c, d, e, f, g, h, i) = E.list id <$> sequenceA
        [ atoEncoding a
        , atoEncoding b
        , atoEncoding c
        , atoEncoding d
        , atoEncoding e
        , atoEncoding f
        , atoEncoding g
        , toH h
        , toI i
        ]
    {-# INLINE aliftToEncoding2 #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f, AToJSON m g, AToJSON m h) => AToJSON1 m ((,,,,,,,,) a b c d e f g h) where
    aliftToEncoding = aliftToEncoding2 atoEncoding atoEncodingList
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f, AToJSON m g, AToJSON m h, AToJSON m i) => AToJSON m (a, b, c, d, e, f, g, h, i) where
    atoEncoding = atoEncoding2
    {-# INLINE atoEncoding #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f, AToJSON m g, AToJSON m h) => AToJSON2 m ((,,,,,,,,,) a b c d e f g h) where
    aliftToEncoding2 toI _ toJ _ (a, b, c, d, e, f, g, h, i, j) = E.list id <$> sequenceA
        [ atoEncoding a
        , atoEncoding b
        , atoEncoding c
        , atoEncoding d
        , atoEncoding e
        , atoEncoding f
        , atoEncoding g
        , atoEncoding h
        , toI i
        , toJ j
        ]
    {-# INLINE aliftToEncoding2 #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f, AToJSON m g, AToJSON m h, AToJSON m i) => AToJSON1 m ((,,,,,,,,,) a b c d e f g h i) where
    aliftToEncoding = aliftToEncoding2 atoEncoding atoEncodingList
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f, AToJSON m g, AToJSON m h, AToJSON m i, AToJSON m j) => AToJSON m (a, b, c, d, e, f, g, h, i, j) where
    atoEncoding = atoEncoding2
    {-# INLINE atoEncoding #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f, AToJSON m g, AToJSON m h, AToJSON m i) => AToJSON2 m ((,,,,,,,,,,) a b c d e f g h i) where
    aliftToEncoding2 toJ _ toK _ (a, b, c, d, e, f, g, h, i, j, k) = E.list id <$> sequenceA
        [ atoEncoding a
        , atoEncoding b
        , atoEncoding c
        , atoEncoding d
        , atoEncoding e
        , atoEncoding f
        , atoEncoding g
        , atoEncoding h
        , atoEncoding i
        , toJ j
        , toK k
        ]
    {-# INLINE aliftToEncoding2 #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f, AToJSON m g, AToJSON m h, AToJSON m i, AToJSON m j) => AToJSON1 m ((,,,,,,,,,,) a b c d e f g h i j) where
    aliftToEncoding = aliftToEncoding2 atoEncoding atoEncodingList
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f, AToJSON m g, AToJSON m h, AToJSON m i, AToJSON m j, AToJSON m k) => AToJSON m (a, b, c, d, e, f, g, h, i, j, k) where
    atoEncoding = atoEncoding2
    {-# INLINE atoEncoding #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f, AToJSON m g, AToJSON m h, AToJSON m i, AToJSON m j) => AToJSON2 m ((,,,,,,,,,,,) a b c d e f g h i j) where
    aliftToEncoding2 toK _ toL _ (a, b, c, d, e, f, g, h, i, j, k, l) = E.list id <$> sequenceA
        [ atoEncoding a
        , atoEncoding b
        , atoEncoding c
        , atoEncoding d
        , atoEncoding e
        , atoEncoding f
        , atoEncoding g
        , atoEncoding h
        , atoEncoding i
        , atoEncoding j
        , toK k
        , toL l
        ]
    {-# INLINE aliftToEncoding2 #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f, AToJSON m g, AToJSON m h, AToJSON m i, AToJSON m j, AToJSON m k) => AToJSON1 m ((,,,,,,,,,,,) a b c d e f g h i j k) where
    aliftToEncoding = aliftToEncoding2 atoEncoding atoEncodingList
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f, AToJSON m g, AToJSON m h, AToJSON m i, AToJSON m j, AToJSON m k, AToJSON m l) => AToJSON m (a, b, c, d, e, f, g, h, i, j, k, l) where
    atoEncoding = atoEncoding2
    {-# INLINE atoEncoding #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f, AToJSON m g, AToJSON m h, AToJSON m i, AToJSON m j, AToJSON m k) => AToJSON2 m ((,,,,,,,,,,,,) a b c d e f g h i j k) where
    aliftToEncoding2 toL _ toM _ (a, b, c, d, e, f, g, h, i, j, k, l, m') = E.list id <$> sequenceA
        [ atoEncoding a
        , atoEncoding b
        , atoEncoding c
        , atoEncoding d
        , atoEncoding e
        , atoEncoding f
        , atoEncoding g
        , atoEncoding h
        , atoEncoding i
        , atoEncoding j
        , atoEncoding k
        , toL l
        , toM m'
        ]
    {-# INLINE aliftToEncoding2 #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f, AToJSON m g, AToJSON m h, AToJSON m i, AToJSON m j, AToJSON m k, AToJSON m l) => AToJSON1 m ((,,,,,,,,,,,,) a b c d e f g h i j k l) where
    aliftToEncoding = aliftToEncoding2 atoEncoding atoEncodingList
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f, AToJSON m g, AToJSON m h, AToJSON m i, AToJSON m j, AToJSON m k, AToJSON m l, AToJSON m m') => AToJSON m (a, b, c, d, e, f, g, h, i, j, k, l, m') where
    atoEncoding = atoEncoding2
    {-# INLINE atoEncoding #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f, AToJSON m g, AToJSON m h, AToJSON m i, AToJSON m j, AToJSON m k, AToJSON m l) => AToJSON2 m ((,,,,,,,,,,,,,) a b c d e f g h i j k l) where
    aliftToEncoding2 toM _ toN _ (a, b, c, d, e, f, g, h, i, j, k, l, m', n) = E.list id <$> sequenceA
        [ atoEncoding a
        , atoEncoding b
        , atoEncoding c
        , atoEncoding d
        , atoEncoding e
        , atoEncoding f
        , atoEncoding g
        , atoEncoding h
        , atoEncoding i
        , atoEncoding j
        , atoEncoding k
        , atoEncoding l
        , toM m'
        , toN n
        ]
    {-# INLINE aliftToEncoding2 #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f, AToJSON m g, AToJSON m h, AToJSON m i, AToJSON m j, AToJSON m k, AToJSON m l, AToJSON m m') => AToJSON1 m ((,,,,,,,,,,,,,) a b c d e f g h i j k l m') where
    aliftToEncoding = aliftToEncoding2 atoEncoding atoEncodingList
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f, AToJSON m g, AToJSON m h, AToJSON m i, AToJSON m j, AToJSON m k, AToJSON m l, AToJSON m m', AToJSON m n) => AToJSON m (a, b, c, d, e, f, g, h, i, j, k, l, m', n) where
    atoEncoding = atoEncoding2
    {-# INLINE atoEncoding #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f, AToJSON m g, AToJSON m h, AToJSON m i, AToJSON m j, AToJSON m k, AToJSON m l, AToJSON m m') => AToJSON2 m ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m') where
    aliftToEncoding2 toN _ toO _ (a, b, c, d, e, f, g, h, i, j, k, l, m', n, o) = E.list id <$> sequenceA
        [ atoEncoding a
        , atoEncoding b
        , atoEncoding c
        , atoEncoding d
        , atoEncoding e
        , atoEncoding f
        , atoEncoding g
        , atoEncoding h
        , atoEncoding i
        , atoEncoding j
        , atoEncoding k
        , atoEncoding l
        , atoEncoding m'
        , toN n
        , toO o
        ]
    {-# INLINE aliftToEncoding2 #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f, AToJSON m g, AToJSON m h, AToJSON m i, AToJSON m j, AToJSON m k, AToJSON m l, AToJSON m m', AToJSON m n) => AToJSON1 m ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m' n) where
    aliftToEncoding = aliftToEncoding2 atoEncoding atoEncodingList
    {-# INLINE aliftToEncoding #-}

instance (AToJSON m a, AToJSON m b, AToJSON m c, AToJSON m d, AToJSON m e, AToJSON m f, AToJSON m g, AToJSON m h, AToJSON m i, AToJSON m j, AToJSON m k, AToJSON m l, AToJSON m m', AToJSON m n, AToJSON m o) => AToJSON m (a, b, c, d, e, f, g, h, i, j, k, l, m', n, o) where
    atoEncoding = atoEncoding2
    {-# INLINE atoEncoding #-}


