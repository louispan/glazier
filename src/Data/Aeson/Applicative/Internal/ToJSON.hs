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

module Data.Aeson.Applicative.Internal.ToJSON where

import Prelude.Compat

import Data.Maybe
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Applicative.Internal.Generic
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
    , null_
    , openBracket
    , retagEncoding
    , string
    , (>*<)
    , (><)
    )
import Data.Aeson.Internal
import Data.Aeson.Types
import Control.Applicative (Const(..))
import Control.Monad.ST (ST)
import Data.Attoparsec.Number (Number(..))
import Data.Bits (unsafeShiftR)
import Data.DList (DList)
import Data.Fixed (Fixed, HasResolution)
import Data.Foldable (toList)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Contravariant (Contravariant (..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(..))
import Data.Functor.Sum (Sum(..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup ((<>))
import Data.Proxy (Proxy(..))
import Data.Ratio (Ratio, denominator, numerator)
import Data.Scientific (Scientific)
import Data.Tagged (Tagged(..))
import Data.Text (Text, pack)
import Data.Time (Day, DiffTime, LocalTime, NominalDiffTime, TimeOfDay, UTCTime, ZonedTime)
import Data.Time.Format (FormatTime, formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Data.Vector (Vector)
import Data.Version (Version, showVersion)
import Data.Void (Void, absurd)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Storable (Storable)
import Foreign.C.Types (CTime (..))
import GHC.Generics
import Numeric.Natural (Natural)
import qualified Data.Aeson.Encoding as E
import qualified Data.Aeson.Encoding.Internal as E (InArray, comma, econcat, retagEncoding)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HashSet
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Monoid as Monoid
import qualified Data.Scientific as Scientific
import qualified Data.Semigroup as Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Tree as Tree
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

import qualified Data.ByteString.Builder as B

import qualified GHC.Exts as Exts
import qualified Data.Primitive.Array as PM
import qualified Data.Primitive.Types as PM

#if MIN_VERSION_primitive(0,6,4)
import qualified Data.Primitive.SmallArray as PM
import qualified Data.Primitive.UnliftedArray as PM
import qualified Data.Primitive.PrimArray as PM
#endif

#if !(MIN_VERSION_bytestring(0,10,0))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (plusPtr)
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy.Internal as L
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
class MGToJSON m enc arity f where
    -- | This method (applied to 'defaultOptions') is used as the
    -- default generic implementation of 'toJSON'
    -- (with @enc ~ 'Value'@ and @arity ~ 'Zero'@)
    -- and 'liftToJSON' (if the @arity@ is 'One').
    --
    -- It also provides a generic implementation of 'toEncoding'
    -- (with @enc ~ 'Encoding'@ and @arity ~ 'Zero'@)
    -- and 'liftToEncoding' (if the @arity@ is 'One').
    mgToJSON :: Options -> MToArgs m enc arity a -> f a -> m enc

-- | A 'ToArgs' value either stores nothing (for 'ToJSON') or it stores the two
-- function arguments that encode occurrences of the type parameter (for
-- 'ToJSON1').
data MToArgs m res arity a where
    MNoToArgs :: MToArgs m res Zero a
    MTo1Args  :: (a -> m res) -> ([a] -> m res) -> MToArgs m res One a

-- | A configurable generic JSON encoder. This function applied to
-- 'defaultOptions' is used as the default for 'mtoEncoding' when the type
-- is an instance of 'Generic'.
mgenericToEncoding :: (Generic a, MGToJSON m Encoding Zero (Rep a))
                  => Options -> a -> m Encoding
mgenericToEncoding opts = mgToJSON opts MNoToArgs . from

-- | A configurable generic JSON encoder. This function applied to
-- 'defaultOptions' is used as the default for 'liftToEncoding' when the type
-- is an instance of 'Generic1'.
mgenericLiftToEncoding :: (Generic1 f, MGToJSON m Encoding One (Rep1 f))
                      => Options -> (a -> m Encoding) -> ([a] -> m Encoding)
                      -> f a -> m Encoding
mgenericLiftToEncoding opts te tel = mgToJSON opts (MTo1Args te tel) . from1

-- -----------------------------------------------------
-- Class
-- -----------------------------------------------------

-- | Analogous to 'toJSON'
class Applicative m => MToJSON m a where
    mtoEncoding :: a -> m Encoding
    {-# INLINE mtoEncoding #-}

    default mtoEncoding :: (Generic a, MGToJSON m Encoding Zero (Rep a)) => a -> m Encoding
    mtoEncoding = mgenericToEncoding defaultOptions

    mtoEncodingList :: [a] -> m Encoding
    mtoEncodingList = mlistEncoding mtoEncoding
    {-# INLINE mtoEncodingList #-}

-------------------------------------------------------------------------------
-- Lifings of FromJSON and ToJSON to unary and binary type constructors
-------------------------------------------------------------------------------

-- | Analogous to 'toJSON1'
class Applicative m => MToJSON1 m f where
    mliftToEncoding :: (a -> m Encoding) -> ([a] -> m Encoding) -> f a -> m Encoding

    default mliftToEncoding :: (Generic1 f, MGToJSON m Encoding One (Rep1 f))
                           => (a -> m Encoding) -> ([a] -> m Encoding)
                           -> f a -> m Encoding
    mliftToEncoding = mgenericLiftToEncoding defaultOptions

    mliftToEncodingList :: (a -> m Encoding) -> ([a] -> m Encoding) -> [f a] -> m Encoding
    mliftToEncodingList f g = mlistEncoding (mliftToEncoding f g)

-- | Lift the standard 'toEncoding' function through the type constructor.
mtoEncoding1 :: (MToJSON1 m f, MToJSON m a) => f a -> m Encoding
mtoEncoding1 = mliftToEncoding mtoEncoding mtoEncodingList
{-# INLINE mtoEncoding1 #-}

-- | Lifting of the 'ToJSON' class to binary type constructors.
--
-- Instead of manually writing your 'ToJSON2' instance, "Data.Aeson.TH"
-- provides Template Haskell functions which will derive an instance at compile time.
--
-- The compiler cannot provide a default generic implementation for 'liftToJSON2',
-- unlike 'toJSON' and 'liftToJSON'.
class Applicative m => MToJSON2 m f where
    mliftToEncoding2 :: (a -> m Encoding) -> ([a] -> m Encoding) -> (b -> m Encoding) -> ([b] -> m Encoding) -> f a b -> m Encoding
    mliftToEncodingList2 :: (a -> m Encoding) -> ([a] -> m Encoding) -> (b -> m Encoding) -> ([b] -> m Encoding) -> [f a b] -> m Encoding
    mliftToEncodingList2 fa ga fb gb = mlistEncoding (mliftToEncoding2 fa ga fb gb)

-- | Lift the standard 'toEncoding' function through the type constructor.
mtoEncoding2 :: (MToJSON2 m f, MToJSON m a, MToJSON m b) => f a b -> m Encoding
mtoEncoding2 = mliftToEncoding2 mtoEncoding mtoEncodingList mtoEncoding mtoEncodingList
{-# INLINE mtoEncoding2 #-}

-------------------------------------------------------------------------------
-- Encoding functions
-------------------------------------------------------------------------------

-- | Helper function to use with 'mliftToEncoding'.
-- Useful when writing own 'MToJSON1' instances.
mlistEncoding :: Applicative m => (a -> m Encoding) -> [a] -> m Encoding
mlistEncoding _  [] = pure emptyArray_
mlistEncoding f (x:xs) = (pure openBracket) `combine` f x `combine` commas xs `combine` (pure closeBracket)
  where
    combine = liftA2 (><)
    commas = foldr (\v vs -> (pure comma) `combine` f v `combine` vs) (pure $ Encoding mempty)
{-# INLINE mlistEncoding #-}

-- | Encode as JSON object
-- Based on Data.Aeson.Encoding.Internal.dict
mdictEncoding
    :: Applicative m
    => (k -> Encoding' Text)                     -- ^ key encoding
    -> (v -> m Encoding)                         -- ^ value encoding
    -> (forall a. (k -> v -> a -> a) -> a -> c -> a)  -- ^ @foldrWithKey@ - indexed fold
    -> c                                              -- ^ container
    -> m Encoding
mdictEncoding encodeKey encodeVal foldrWithKey = foldrWithKey go (pure $ Encoding mempty)
  where
    combine = liftA2 (><)
    go k v c = encodeKV k v `combine` (pure comma) `combine` c
    encodeKV k v = (pure . retagEncoding $ encodeKey k) `combine` (pure colon) `combine` (retagEncoding <$> encodeVal v)
{-# INLINE mdictEncoding #-}

-------------------------------------------------------------------------------
-- [] instances
-------------------------------------------------------------------------------

instance Applicative m => MToJSON1 m [] where
    mliftToEncoding _ p' = p'
    {-# INLINE mliftToEncoding #-}

instance MToJSON m a => MToJSON m [a] where
    {-# SPECIALIZE instance Applicative m => MToJSON m String #-}
    {-# SPECIALIZE instance Applicative m => MToJSON m [String] #-}
    {-# SPECIALIZE instance Applicative m => MToJSON m [Array] #-}
    {-# SPECIALIZE instance Applicative m => MToJSON m [Object] #-}

    mtoEncoding = mtoEncoding1
    {-# INLINE mtoEncoding #-}

-------------------------------------------------------------------------------
-- Generic toJSON / toEncoding
-------------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} (MGToJSON m enc arity a) => MGToJSON m enc arity (M1 i c a) where
    -- Meta-information, which is not handled elsewhere, is ignored:
    mgToJSON opts targs = mgToJSON opts targs . unM1
    {-# INLINE mgToJSON #-}

instance MGToJSON m enc One Par1 where
    -- Direct occurrences of the last type parameter are encoded with the
    -- function passed in as an argument:
    mgToJSON _opts (MTo1Args tj _) = tj . unPar1
    {-# INLINE mgToJSON #-}

instance ( MConsToJSON m enc arity a
         , AllNullary (C1 c a) allNullary
         , MSumToJSON m enc arity (C1 c a) allNullary
         ) => MGToJSON m enc arity (D1 d (C1 c a)) where
    -- The option 'tagSingleConstructors' determines whether to wrap
    -- a single-constructor type.
    mgToJSON opts targs
#if MIN_VERSION_aeson(1,3,0)
        | tagSingleConstructors opts = (unTagged :: Tagged allNullary (m enc) -> m enc)
                                     . msumToJSON opts targs
                                     . unM1
#endif
        | otherwise = mconsToJSON opts targs . unM1 . unM1
    {-# INLINE mgToJSON #-}

instance (MConsToJSON m enc arity a) => MGToJSON m enc arity (C1 c a) where
    -- Constructors need to be encoded differently depending on whether they're
    -- a record or not. This distinction is made by 'consToJSON':
    mgToJSON opts targs = mconsToJSON opts targs . unM1
    {-# INLINE mgToJSON #-}

instance ( AllNullary (a :+: b) allNullary
         , MSumToJSON  m enc arity (a :+: b) allNullary
         ) => MGToJSON m enc arity (a :+: b)
  where
    -- If all constructors of a sum datatype are nullary and the
    -- 'allNullaryToStringTag' option is set they are encoded to
    -- strings.  This distinction is made by 'sumToJSON':
    mgToJSON opts targs = (unTagged :: Tagged allNullary (m enc) -> m enc)
                       . msumToJSON opts targs
    {-# INLINE mgToJSON #-}

--------------------------------------------------------------------------------
-- Generic toEncoding

instance MToJSON m a => MGToJSON m Encoding arity (K1 i a) where
    -- Constant values are encoded using their ToJSON instance:
    mgToJSON _opts _ = mtoEncoding . unK1
    {-# INLINE mgToJSON #-}

instance MToJSON1 m f => MGToJSON m Encoding One (Rec1 f) where
    -- Recursive occurrences of the last type parameter are encoded using their
    -- ToEncoding1 instance:
    mgToJSON _opts (MTo1Args te tel) = mliftToEncoding te tel . unRec1
    {-# INLINE mgToJSON #-}

instance Applicative m => MGToJSON m Encoding arity U1 where
    -- Empty constructors are encoded to an empty array:
    mgToJSON _opts _ _ = pure emptyArray_
    {-# INLINE mgToJSON #-}

instance ( Applicative m
         , MEncodeProduct m arity a
         , MEncodeProduct m arity b
         ) => MGToJSON m Encoding arity (a :*: b)
  where
    -- Products are encoded to an array. Here we allocate a mutable vector of
    -- the same size as the product and write the product's elements to it using
    -- 'encodeProduct':
    mgToJSON opts targs p = fmap (list retagEncoding) $ sequenceA [mencodeProduct opts targs p]
    {-# INLINE mgToJSON #-}

instance ( MToJSON1 m f
         , MGToJSON m Encoding One g
         ) => MGToJSON m Encoding One (f :.: g)
  where
    -- If an occurrence of the last type parameter is nested inside two
    -- composed types, it is encoded by using the outermost type's ToJSON1
    -- instance to generically encode the innermost type:
    mgToJSON opts targs =
      let gte = mgToJSON opts targs in
      mliftToEncoding gte (mlistEncoding gte) . unComp1
    {-# INLINE mgToJSON #-}

--------------------------------------------------------------------------------

class MSumToJSON m enc arity f allNullary where
    msumToJSON :: Options -> MToArgs m enc arity a
              -> f a -> Tagged allNullary (m enc)

instance
    ( Applicative m
    , GetConName f
    , FromString enc
    , MTaggedObject m enc arity f
    , MSumToJSON' m ObjectWithSingleField enc arity f
    , MSumToJSON' m TwoElemArray enc arity f
    , MSumToJSON' m UntaggedValue enc arity f
    ) => MSumToJSON m enc arity f 'True
  where
    msumToJSON opts targs
        | allNullaryToStringTag opts = Tagged . pure . fromString
                                     . constructorTagModifier opts . getConName
        | otherwise = Tagged . mnonAllNullarySumToJSON opts targs

instance ( MTaggedObject m enc arity f
         , MSumToJSON' m ObjectWithSingleField enc arity f
         , MSumToJSON' m TwoElemArray enc arity f
         , MSumToJSON' m UntaggedValue enc arity f
         ) => MSumToJSON m enc arity f 'False
  where
    msumToJSON opts targs = Tagged . mnonAllNullarySumToJSON opts targs

mnonAllNullarySumToJSON ::
    ( MTaggedObject m enc arity f
    , MSumToJSON' m ObjectWithSingleField enc arity f
    , MSumToJSON' m TwoElemArray enc arity f
    , MSumToJSON' m UntaggedValue enc arity f
    ) => Options -> MToArgs m enc arity a
    -> f a -> m enc
mnonAllNullarySumToJSON opts targs =
    case sumEncoding opts of

      TaggedObject{..}      ->
        mtaggedObject opts targs tagFieldName contentsFieldName

      ObjectWithSingleField ->
        (unTagged :: Tagged ObjectWithSingleField (m enc) -> m enc)
          . msumToJSON' opts targs

      TwoElemArray          ->
        (unTagged :: Tagged TwoElemArray (m enc) -> m enc)
          . msumToJSON' opts targs

      UntaggedValue         ->
        (unTagged :: Tagged UntaggedValue (m enc) -> m enc)
          . msumToJSON' opts targs

--------------------------------------------------------------------------------

class FromString enc where
  fromString :: String -> enc

instance FromString Encoding where
  fromString = toEncoding

instance FromString Value where
  fromString = String . pack

--------------------------------------------------------------------------------

class MTaggedObject m enc arity f where
    mtaggedObject :: Options -> MToArgs m enc arity a
                 -> String -> String
                 -> f a -> m enc

instance ( MTaggedObject m enc arity a
         , MTaggedObject m enc arity b
         ) => MTaggedObject m enc arity (a :+: b)
  where
    mtaggedObject opts targs tagFieldName contentsFieldName (L1 x) =
        mtaggedObject opts targs tagFieldName contentsFieldName x
    mtaggedObject opts targs tagFieldName contentsFieldName (R1 x) =
        mtaggedObject opts targs tagFieldName contentsFieldName x

instance
    ( Functor m
    , IsRecord a isRecord
    , MTaggedObject' m enc pairs arity a isRecord
    , FromPairs enc pairs
    , FromString enc
    , KeyValuePair enc pairs
    , Constructor c
    ) => MTaggedObject m enc arity (C1 c a)
  where
    mtaggedObject opts targs tagFieldName contentsFieldName =
      fmap (fromPairs . mappend tag) . contents
      where
        tag = tagFieldName `pair`
          (fromString (constructorTagModifier opts (conName (undefined :: t c a p)))
            :: enc)
        contents =
          (unTagged :: Tagged isRecord (m pairs) -> m pairs) .
            mtaggedObject' opts targs contentsFieldName . unM1

class MTaggedObject' m enc pairs arity f isRecord where
    mtaggedObject' :: Options -> MToArgs m enc arity a
                  -> String -> f a -> Tagged isRecord (m pairs)

instance
    ( Functor m
    , MGToJSON m enc arity f
    , KeyValuePair enc pairs
    ) => MTaggedObject' m enc pairs arity f 'False
  where
    mtaggedObject' opts targs contentsFieldName =
        Tagged . fmap (contentsFieldName `pair`) . mgToJSON opts targs

instance {-# OVERLAPPING #-} (Applicative m, Monoid pairs) => MTaggedObject' m enc pairs arity U1 'False where
    mtaggedObject' _ _ _ _ = Tagged (pure mempty)

instance ( MRecordToPairs m enc pairs arity f
         ) => MTaggedObject' m enc pairs arity f 'True
  where
    mtaggedObject' opts targs _ = Tagged . mrecordToPairs opts targs

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

class MSumToJSON' m s enc arity f where
    msumToJSON' :: Options -> MToArgs m enc arity a
                    -> f a -> Tagged s (m enc)

instance ( MSumToJSON' m s enc arity a
         , MSumToJSON' m s enc arity b
         ) => MSumToJSON' m s enc arity (a :+: b)
  where
    msumToJSON' opts targs (L1 x) = msumToJSON' opts targs x
    msumToJSON' opts targs (R1 x) = msumToJSON' opts targs x

--------------------------------------------------------------------------------

instance
    ( Applicative m
    , MGToJSON m Encoding arity a
    , MConsToJSON m Encoding arity a
    , Constructor c
    ) => MSumToJSON' m TwoElemArray Encoding arity (C1 c a)
  where
    msumToJSON' opts targs x = Tagged $ fmap (list id) $ sequenceA
      [ mtoEncoding (constructorTagModifier opts (conName (undefined :: t c a p)))
      , mgToJSON opts targs x
      ]

--------------------------------------------------------------------------------

class MConsToJSON m enc arity f where
    mconsToJSON :: Options -> MToArgs m enc arity a
               -> f a -> m enc

class MConsToJSON' m enc arity f isRecord where
    mconsToJSON' :: Options -> MToArgs m enc arity a
                    -> f a -> Tagged isRecord (m enc)

instance ( IsRecord f isRecord
         , MConsToJSON' m enc arity f isRecord
         ) => MConsToJSON m enc arity f
  where
    mconsToJSON opts targs =
        (unTagged :: Tagged isRecord (m enc) -> m enc)
      . mconsToJSON' opts targs
    {-# INLINE mconsToJSON #-}

instance {-# OVERLAPPING #-}
         ( Functor m
         , MRecordToPairs m enc pairs arity (S1 s f)
         , FromPairs enc pairs
         , MGToJSON m enc arity f
         ) => MConsToJSON' m enc arity (S1 s f) 'True
  where
    mconsToJSON' opts targs
      | unwrapUnaryRecords opts = Tagged . mgToJSON opts targs
      | otherwise = Tagged . fmap fromPairs . mrecordToPairs opts targs
    {-# INLINE mconsToJSON' #-}

instance
    ( Functor m
    , MRecordToPairs m enc pairs arity f
    , FromPairs enc pairs
    ) => MConsToJSON' m enc arity f 'True
  where
    mconsToJSON' opts targs = Tagged . fmap fromPairs . mrecordToPairs opts targs
    {-# INLINE mconsToJSON' #-}

instance MGToJSON m enc arity f => MConsToJSON' m enc arity f 'False where
    mconsToJSON' opts targs = Tagged . mgToJSON opts targs
    {-# INLINE mconsToJSON' #-}

--------------------------------------------------------------------------------

class MRecordToPairs m enc pairs arity f where
    -- 1st element: whole thing
    -- 2nd element: in case the record has only 1 field, just the value
    --              of the field (without the key); 'Nothing' otherwise
    mrecordToPairs :: Options -> MToArgs m enc arity a
                  -> f a -> m pairs

instance ( Applicative m
         , Monoid pairs
         , MRecordToPairs m enc pairs arity a
         , MRecordToPairs m enc pairs arity b
         ) => MRecordToPairs m enc pairs arity (a :*: b)
  where
    mrecordToPairs opts (targs :: MToArgs m enc arity p) (a :*: b) =
        mappend <$> pairsOf a <*> pairsOf b
      where
        pairsOf :: (MRecordToPairs m enc pairs arity f) => f p -> m pairs
        pairsOf = mrecordToPairs opts targs
    {-# INLINE mrecordToPairs #-}

instance
    ( Applicative m
    , Selector s
    , MGToJSON m enc arity a
    , KeyValuePair enc pairs
    ) => MRecordToPairs m enc pairs arity (S1 s a)
  where
    mrecordToPairs = mfieldToPair
    {-# INLINE mrecordToPairs #-}

instance {-# INCOHERENT #-}
    ( Applicative m
    , Selector s
    , MGToJSON m enc arity (K1 i (Maybe a))
    , KeyValuePair enc pairs
    , Monoid pairs
    ) => MRecordToPairs m enc pairs arity (S1 s (K1 i (Maybe a)))
  where
    mrecordToPairs opts _ (M1 k1) | omitNothingFields opts
                                 , K1 Nothing <- k1 = pure mempty
    mrecordToPairs opts targs m1 = mfieldToPair opts targs m1
    {-# INLINE mrecordToPairs #-}

instance {-# INCOHERENT #-}
    ( Applicative m
    , Selector s
    , MGToJSON m enc arity (K1 i (Maybe a))
    , KeyValuePair enc pairs
    , Monoid pairs
    ) => MRecordToPairs m enc pairs arity (S1 s (K1 i (Semigroup.Option a)))
  where
    mrecordToPairs opts targs = mrecordToPairs opts targs . unwrap
      where
        unwrap :: S1 s (K1 i (Semigroup.Option a)) p -> S1 s (K1 i (Maybe a)) p
        unwrap (M1 (K1 (Semigroup.Option a))) = M1 (K1 a)
    {-# INLINE mrecordToPairs #-}

mfieldToPair :: ( Applicative m
               , Selector s
               , MGToJSON m enc arity a
               , KeyValuePair enc pairs)
            => Options -> MToArgs m enc arity p
            -> S1 s a p -> m pairs
mfieldToPair opts targs m1 =
  let key   = fieldLabelModifier opts (selName m1)
      value = mgToJSON opts targs (unM1 m1)
  in pair <$> (pure key) <*> value
{-# INLINE mfieldToPair #-}

--------------------------------------------------------------------------------

class MEncodeProduct m arity f where
    mencodeProduct :: Options -> MToArgs m Encoding arity a
                  -> f a -> m (Encoding' InArray)

instance
    ( Applicative m
    , MEncodeProduct m arity a
    , MEncodeProduct m arity b
    ) => MEncodeProduct m arity (a :*: b) where
    mencodeProduct opts targs (a :*: b) | omitNothingFields opts =
        fmap (econcat . intersperse comma .
            filter (not . nullEncoding))
        $ sequenceA [mencodeProduct opts targs a, mencodeProduct opts targs b]
    mencodeProduct opts targs (a :*: b) = (>*<) <$>
      mencodeProduct opts targs a <*>
      mencodeProduct opts targs b
    {-# INLINE mencodeProduct #-}

instance {-# OVERLAPPABLE #-} (Functor m, MGToJSON m Encoding arity a) => MEncodeProduct m arity a where
    mencodeProduct opts targs a = retagEncoding <$> mgToJSON opts targs a
    {-# INLINE mencodeProduct #-}

------------------------------------------------------------------------------

instance ( Functor m
         , MGToJSON m enc arity a
         , MConsToJSON m enc arity a
         , FromPairs enc pairs
         , KeyValuePair enc pairs
         , Constructor c
         ) => MSumToJSON' m ObjectWithSingleField enc arity (C1 c a)
  where
    msumToJSON' opts targs =
      Tagged . fmap (fromPairs . (typ `pair`)) . mgToJSON opts targs
        where
          typ = constructorTagModifier opts $
                         conName (undefined :: t c a p)

--------------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-}
    ( MConsToJSON m enc arity a
    ) => MSumToJSON' m UntaggedValue enc arity (C1 c a)
  where
    msumToJSON' opts targs = Tagged . mgToJSON opts targs

instance {-# OVERLAPPING #-}
    ( Applicative m
    , Constructor c
    , FromString enc
    ) => MSumToJSON' m UntaggedValue enc arity (C1 c U1)
  where
    msumToJSON' opts _ _ = Tagged . pure . fromString $
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

-- -------------------------------------------------------------------------------
-- -- base
-- -------------------------------------------------------------------------------

-- instance Applicative m => MToJSON2 m Const where
--     mliftToEncoding2 t _ _ _ (Const x) = t x
--     {-# INLINE mliftToEncoding2 #-}

-- instance MToJSON m a => MToJSON1 m (Const a) where
--     mliftToEncoding _ _ (Const x) = mtoEncoding x
--     {-# INLINE mliftToEncoding #-}

-- instance MToJSON m a => MToJSON m (Const a b) where
--     mtoEncoding (Const x) = mtoEncoding x
--     {-# INLINE mtoEncoding #-}


-- instance MToJSON1 m Maybe where
--     mliftToEncoding t _ (Just a) = t a
--     mliftToEncoding _  _ Nothing  = E.null_
--     {-# INLINE mliftToEncoding #-}

-- instance (MToJSON m a) => MToJSON m (Maybe a) where
--     mtoEncoding = mtoEncoding1
--     {-# INLINE mtoEncoding #-}


-- instance Applicative m => MToJSON2 m Either where
--     mliftToEncoding2  toA _ _toB _ (Left a) = E.pairs $ E.pair "Left" $ toA a

--     mliftToEncoding2 _toA _ toB _ (Right b) = E.pairs $ E.pair "Right" $ toB b
--     {-# INLINE mliftToEncoding2 #-}

-- instance (MToJSON m a) => MToJSON1 m (Either a) where
--     mliftToEncoding = mliftToEncoding2 mtoEncoding mtoEncodingList
--     {-# INLINE mliftToEncoding #-}

-- instance (MToJSON m a, MToJSON m b) => MToJSON m (Either a b) where
--     mtoEncoding = mtoEncoding2
--     {-# INLINE mtoEncoding #-}


-- instance Applicative m => MToJSON m Void where
--     mtoEncoding = pure . toEncoding
--     {-# INLINE mtoEncoding #-}


instance Applicative m => MToJSON m Bool where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}


instance Applicative m => MToJSON m Ordering where
    mtoEncoding = pure . toEncoding

instance Applicative m => MToJSON m () where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}

instance Applicative m => MToJSON m Char where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}

    mtoEncodingList = pure . toEncodingList
    {-# INLINE mtoEncodingList #-}


instance Applicative m => MToJSON m Double where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}


instance Applicative m => MToJSON m Number where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}


instance Applicative m => MToJSON m Float where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}


-- instance (MToJSON m a, Integral a) => MToJSON m (Ratio a) where
--     mtoEncoding r = E.pairs $
--         "numerator" .= numerator r <>
--         "denominator" .= denominator r
--     {-# INLINE mtoEncoding #-}


instance (Applicative m, HasResolution a) => MToJSON m (Fixed a) where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}


instance Applicative m => MToJSON m Int where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}


instance Applicative m => MToJSON m Integer where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}


instance Applicative m => MToJSON m Natural where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}


instance Applicative m => MToJSON m Int8 where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}


instance Applicative m => MToJSON m Int16 where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}


instance Applicative m => MToJSON m Int32 where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}


instance Applicative m => MToJSON m Int64 where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}


instance Applicative m => MToJSON m Word where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}


instance Applicative m => MToJSON m Word8 where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}


instance Applicative m => MToJSON m Word16 where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}


instance Applicative m => MToJSON m Word32 where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}

instance Applicative m => MToJSON m Word64 where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}

-- instance Applicative m => MToJSON m CTime where
--     mtoEncoding = pure . toEncoding
--     {-# INLINE mtoEncoding #-}


instance Applicative m => MToJSON m Text where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}


instance Applicative m => MToJSON m LT.Text where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}


instance Applicative m => MToJSON m Version where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}

-- -------------------------------------------------------------------------------
-- -- semigroups NonEmpty
-- -------------------------------------------------------------------------------

-- instance MToJSON1 m NonEmpty where
--     mliftToEncoding t _ = listEncoding t . NE.toList
--     {-# INLINE mliftToEncoding #-}

-- instance (MToJSON m a) => MToJSON m (NonEmpty a) where
--     mtoEncoding = mtoEncoding1
--     {-# INLINE mtoEncoding #-}

-------------------------------------------------------------------------------
-- scientific
-------------------------------------------------------------------------------

instance Applicative m => MToJSON m Scientific where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}

-- -------------------------------------------------------------------------------
-- -- DList
-- -------------------------------------------------------------------------------

-- instance MToJSON1 m DList.DList where
--     mliftToEncoding t _ = listEncoding t . toList
--     {-# INLINE mliftToEncoding #-}

-- instance (MToJSON m a) => MToJSON m (DList.DList a) where
--     mtoEncoding = mtoEncoding1
--     {-# INLINE mtoEncoding #-}

-- -------------------------------------------------------------------------------
-- -- transformers - Functors
-- -------------------------------------------------------------------------------

-- instance MToJSON1 m Identity where
--     mliftToEncoding t _ (Identity a) = t a
--     {-# INLINE mliftToEncoding #-}

--     mliftToEncodingList _ tl xs = tl (map runIdentity xs)
--     {-# INLINE mliftToEncodingList #-}

-- instance (MToJSON m a) => MToJSON m (Identity a) where
--     mtoEncoding = mtoEncoding1
--     {-# INLINE mtoEncoding #-}

--     mtoEncodingList = mliftToEncodingList mtoEncoding mtoEncodingList
--     {-# INLINE mtoEncodingList #-}


-- instance (ToJSON1 f, MToJSON1 m g) => MToJSON1 m (Compose f g) where
--     mliftToEncoding te tel (Compose x) = mliftToEncoding g gl x
--       where
--         g = mliftToEncoding te tel
--         gl = mliftToEncodingList te tel
--     {-# INLINE mliftToEncoding #-}

--     mliftToEncodingList te tel xs = mliftToEncodingList g gl (map getCompose xs)
--       where
--         g = mliftToEncoding te tel
--         gl = mliftToEncodingList te tel
--     {-# INLINE mliftToEncodingList #-}

-- instance (ToJSON1 f, MToJSON1 m g, MToJSON m a) => MToJSON m (Compose f g a) where
--     mtoEncoding = mtoEncoding1
--     {-# INLINE mtoEncoding #-}

--     mtoEncodingList = mliftToEncodingList mtoEncoding mtoEncodingList
--     {-# INLINE mtoEncodingList #-}


-- instance (ToJSON1 f, MToJSON1 m g) => MToJSON1 m (Product f g) where
--     mliftToEncoding te tel (Pair x y) = mliftToEncoding2 tx txl ty tyl (x, y)
--       where
--         tx = mliftToEncoding te tel
--         txl = mliftToEncodingList te tel
--         ty = mliftToEncoding te tel
--         tyl = mliftToEncodingList te tel

-- instance (ToJSON1 f, MToJSON1 m g, MToJSON m a) => MToJSON m (Product f g a) where
--     mtoEncoding = mtoEncoding1
--     {-# INLINE mtoEncoding #-}

-- instance (ToJSON1 f, MToJSON1 m g) => MToJSON1 m (Sum f g) where
--     mliftToEncoding te tel (InL x) = E.pairs $ E.pair "InL" $ mliftToEncoding te tel x
--     mliftToEncoding te tel (InR y) = E.pairs $ E.pair "InR" $ mliftToEncoding te tel y

-- instance (ToJSON1 f, MToJSON1 m g, MToJSON m a) => MToJSON m (Sum f g a) where
--     mtoEncoding = mtoEncoding1
--     {-# INLINE mtoEncoding #-}

-- -------------------------------------------------------------------------------
-- -- containers
-- -------------------------------------------------------------------------------

-- instance MToJSON1 m Seq.Seq where
--     mliftToEncoding t _ = listEncoding t . toList
--     {-# INLINE mliftToEncoding #-}

-- instance (MToJSON m a) => MToJSON m (Seq.Seq a) where
--     mtoEncoding = mtoEncoding1
--     {-# INLINE mtoEncoding #-}


-- instance MToJSON1 m Set.Set where
--     mliftToEncoding t _ = listEncoding t . Set.toList
--     {-# INLINE mliftToEncoding #-}

-- instance (MToJSON m a) => MToJSON m (Set.Set a) where
--     mtoEncoding = mtoEncoding1
--     {-# INLINE mtoEncoding #-}


-- instance Applicative m => MToJSON m IntSet.IntSet where
--     mtoEncoding = pure . toEncoding
--     {-# INLINE mtoEncoding #-}


-- instance MToJSON1 m IntMap.IntMap where
--     mliftToEncoding t tol = mliftToEncoding to' tol' . IntMap.toList
--       where
--         to'  = mliftToEncoding2     mtoEncoding mtoEncodingList t tol
--         tol' = mliftToEncodingList2 mtoEncoding mtoEncodingList t tol
--     {-# INLINE mliftToEncoding #-}

-- instance MToJSON m a => MToJSON m (IntMap.IntMap a) where
--     mtoEncoding = mtoEncoding1
--     {-# INLINE mtoEncoding #-}

instance (Applicative m, ToJSONKey k) => MToJSON1 m (M.Map k) where
    mliftToEncoding g _ = case toJSONKey of
        ToJSONKeyText _ f -> mdictEncoding f g M.foldrWithKey
        ToJSONKeyValue _ f -> mlistEncoding (pairEncoding f) . M.toList
      where
        pairEncoding f (a, b) = mlistEncoding id [pure $ f a, g b]
    {-# INLINE mliftToEncoding #-}

instance (MToJSON m v, ToJSONKey k) => MToJSON m (M.Map k v) where
    mtoEncoding = mtoEncoding1
    {-# INLINE mtoEncoding #-}


-- instance MToJSON1 m Tree.Tree where
--     mliftToEncoding t tol = go
--       where
--         go (Tree.Node root branches) =
--             mliftToEncoding2 t tol to' tol' (root, branches)

--         to' = mliftToEncoding go (listEncoding go)
--         tol' = mliftToEncodingList go (listEncoding go)
--     {-# INLINE mliftToEncoding #-}

-- instance (MToJSON m v) => MToJSON m (Tree.Tree v) where
--     mtoEncoding = mtoEncoding1
--     {-# INLINE mtoEncoding #-}

-- -------------------------------------------------------------------------------
-- -- uuid
-- -------------------------------------------------------------------------------

-- instance Applicative m => MToJSON m UUID.UUID where
--     mtoEncoding = pure . toEncoding

-- -------------------------------------------------------------------------------
-- -- vector
-- -------------------------------------------------------------------------------

instance Applicative m => MToJSON1 m Vector where
    mliftToEncoding t _ =  mlistEncoding t . V.toList
    {-# INLINE mliftToEncoding #-}

instance (MToJSON m a) => MToJSON m (Vector a) where
    {-# SPECIALIZE instance Applicative m => MToJSON m Array #-}

    mtoEncoding = mtoEncoding1
    {-# INLINE mtoEncoding #-}

-- encodeVector :: (MToJSON m a, VG.Vector v a) => v a -> Encoding
-- encodeVector = listEncoding mtoEncoding . VG.toList
-- {-# INLINE encodeVector #-}

-- vectorToJSON :: (VG.Vector v a, MToJSON m a) => v a -> Value
-- vectorToJSON = Array . V.map toJSON . V.convert
-- {-# INLINE vectorToJSON #-}

-- instance (Storable a, MToJSON m a) => MToJSON m (VS.Vector a) where
--     mtoEncoding = encodeVector
--     {-# INLINE mtoEncoding #-}


-- instance (VP.Prim a, MToJSON m a) => MToJSON m (VP.Vector a) where
--     mtoEncoding = encodeVector
--     {-# INLINE mtoEncoding #-}


-- instance (VG.Vector VU.Vector a, MToJSON m a) => MToJSON m (VU.Vector a) where
--     mtoEncoding = encodeVector
--     {-# INLINE mtoEncoding #-}

-- -------------------------------------------------------------------------------
-- -- unordered-containers
-- -------------------------------------------------------------------------------

-- instance MToJSON1 m HashSet.HashSet where
--     mliftToEncoding t _ = listEncoding t . HashSet.toList
--     {-# INLINE mliftToEncoding #-}

-- instance (MToJSON m a) => MToJSON m (HashSet.HashSet a) where
--     mtoEncoding = mtoEncoding1
--     {-# INLINE mtoEncoding #-}

instance (Applicative m, ToJSONKey k) => MToJSON1 m (H.HashMap k) where
    mliftToEncoding g _ = case toJSONKey of
        ToJSONKeyText _ f -> mdictEncoding f g H.foldrWithKey
        ToJSONKeyValue _ f -> mlistEncoding (pairEncoding f) . H.toList
      where
        pairEncoding f (a, b) = mlistEncoding id [pure $ f a, g b]
    {-# INLINE mliftToEncoding #-}

instance (MToJSON m v, ToJSONKey k) => MToJSON m (H.HashMap k v) where
    {-# SPECIALIZE instance Applicative m => MToJSON m Object #-}

    mtoEncoding = mtoEncoding1
    {-# INLINE mtoEncoding #-}

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

instance Applicative m => MToJSON m Value where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}

instance Applicative m => MToJSON m DotNetTime where
    mtoEncoding = pure . toEncoding

-- -------------------------------------------------------------------------------
-- -- primitive
-- -------------------------------------------------------------------------------

-- #if MIN_VERSION_base(4,7,0)
-- instance Applicative m => MToJSON m a => MToJSON m (PM.Array a) where
--   -- note: we could do better than this if vector exposed the data
--   -- constructor in Data.Vector.
--   mtoEncoding = mtoEncoding . Exts.toList

-- #if (MIN_VERSION_primitive(0,6,4))
-- instance Applicative m => MToJSON m a => MToJSON m (PM.SmallArray a) where
--   mtoEncoding = mtoEncoding . Exts.toList

-- instance (PM.Prim a,ToJSON a) => MToJSON m (PM.PrimArray a) where
--   mtoEncoding = mtoEncoding . Exts.toList

-- instance (PM.PrimUnlifted a,ToJSON a) => MToJSON m (PM.UnliftedArray a) where
--   mtoEncoding = mtoEncoding . Exts.toList
-- #endif
-- #endif

-- -------------------------------------------------------------------------------
-- -- time
-- -------------------------------------------------------------------------------

-- instance Applicative m => MToJSON m Day where
--     mtoEncoding = pure . toEncoding

-- instance Applicative m => MToJSON m TimeOfDay where
--     mtoEncoding = pure . toEncoding


-- instance Applicative m => MToJSON m LocalTime where
--     mtoEncoding = pure . toEncoding


-- instance Applicative m => MToJSON m ZonedTime where
--     mtoEncoding = pure . toEncoding


-- instance Applicative m => MToJSON m UTCTime where
--     mtoEncoding = pure . toEncoding

-- instance Applicative m => MToJSON m NominalDiffTime where
--     mtoEncoding = pure . toEncoding
--     {-# INLINE mtoEncoding #-}


-- instance Applicative m => MToJSON m DiffTime where
--     mtoEncoding = pure . toEncoding
--     {-# INLINE mtoEncoding #-}

-- -------------------------------------------------------------------------------
-- -- base Monoid/Semigroup
-- -------------------------------------------------------------------------------

-- instance MToJSON1 m Monoid.Dual where
--     mliftToEncoding t _ = t . Monoid.getDual
--     {-# INLINE mliftToEncoding #-}

-- instance MToJSON m a => MToJSON m (Monoid.Dual a) where
--     mtoEncoding = mtoEncoding1
--     {-# INLINE mtoEncoding #-}


-- instance MToJSON1 m Monoid.First where
--     mliftToEncoding t to' = mliftToEncoding t to' . Monoid.getFirst
--     {-# INLINE mliftToEncoding #-}

-- instance MToJSON m a => MToJSON m (Monoid.First a) where
--     mtoEncoding = mtoEncoding1
--     {-# INLINE mtoEncoding #-}


-- instance MToJSON1 m Monoid.Last where
--     mliftToEncoding t to' = mliftToEncoding t to' . Monoid.getLast
--     {-# INLINE mliftToEncoding #-}

-- instance MToJSON m a => MToJSON m (Monoid.Last a) where
--     mtoEncoding = mtoEncoding1
--     {-# INLINE mtoEncoding #-}


-- instance MToJSON1 m Semigroup.Min where
--     mliftToEncoding t _ (Semigroup.Min x) = t x
--     {-# INLINE mliftToEncoding #-}

-- instance MToJSON m a => MToJSON m (Semigroup.Min a) where
--     mtoEncoding = mtoEncoding1
--     {-# INLINE mtoEncoding #-}


-- instance MToJSON1 m Semigroup.Max where
--     mliftToEncoding t _ (Semigroup.Max x) = t x
--     {-# INLINE mliftToEncoding #-}

-- instance MToJSON m a => MToJSON m (Semigroup.Max a) where
--     mtoEncoding = mtoEncoding1
--     {-# INLINE mtoEncoding #-}

-- instance MToJSON1 m Semigroup.First where
--     mliftToEncoding t _ (Semigroup.First x) = t x
--     {-# INLINE mliftToEncoding #-}

-- instance MToJSON m a => MToJSON m (Semigroup.First a) where
--     mtoEncoding = mtoEncoding1
--     {-# INLINE mtoEncoding #-}


-- instance MToJSON1 m Semigroup.Last where
--     mliftToEncoding t _ (Semigroup.Last x) = t x
--     {-# INLINE mliftToEncoding #-}

-- instance MToJSON m a => MToJSON m (Semigroup.Last a) where
--     mtoEncoding = mtoEncoding1
--     {-# INLINE mtoEncoding #-}


-- instance MToJSON1 m Semigroup.WrappedMonoid where
--     mliftToEncoding t _ (Semigroup.WrapMonoid x) = t x
--     {-# INLINE mliftToEncoding #-}

-- instance MToJSON m a => MToJSON m (Semigroup.WrappedMonoid a) where
--     mtoEncoding = mtoEncoding1
--     {-# INLINE mtoEncoding #-}


-- instance MToJSON1 m Semigroup.Option where
--     mliftToEncoding t to' = mliftToEncoding t to' . Semigroup.getOption
--     {-# INLINE mliftToEncoding #-}

-- instance MToJSON m a => MToJSON m (Semigroup.Option a) where
--     mtoEncoding = mtoEncoding1
--     {-# INLINE mtoEncoding #-}

-- -------------------------------------------------------------------------------
-- -- tagged
-- -------------------------------------------------------------------------------

-- instance MToJSON1 m Proxy where
--     mliftToEncoding _ _ _ = E.null_
--     {-# INLINE mliftToEncoding #-}

-- instance Applicative m => MToJSON m (Proxy a) where
--     mtoEncoding = pure . toEncoding
--     {-# INLINE mtoEncoding #-}


-- instance Applicative m => MToJSON2 m Tagged where
--     mliftToEncoding2 _ _ t _ (Tagged x) = t x
--     {-# INLINE mliftToEncoding2 #-}

-- instance MToJSON1 m (Tagged a) where
--     mliftToEncoding t _ (Tagged x) = t x
--     {-# INLINE mliftToEncoding #-}

-- instance MToJSON m b => MToJSON m (Tagged a b) where
--     mtoEncoding = mtoEncoding1
--     {-# INLINE mtoEncoding #-}


-- -------------------------------------------------------------------------------
-- -- Tuple instances
-- -------------------------------------------------------------------------------

-- instance Applicative m => MToJSON2 m (,) where
--     mliftToEncoding2 toA _ toB _ (a, b) = E.list id [toA a, toB b]
--     {-# INLINE mliftToEncoding2 #-}

-- instance (MToJSON m a) => MToJSON1 m ((,) a) where
--     mliftToEncoding = mliftToEncoding2 mtoEncoding mtoEncodingList
--     {-# INLINE mliftToEncoding #-}

-- instance (MToJSON m a, MToJSON m b) => MToJSON m (a, b) where
--     mtoEncoding = mtoEncoding2
--     {-# INLINE mtoEncoding #-}

-- instance (MToJSON m a) => ToJSON2 ((,,) a) where
--     mliftToEncoding2 toB _ toC _ (a, b, c) = E.list id
--       [ mtoEncoding a
--       , toB b
--       , toC c
--       ]
--     {-# INLINE mliftToEncoding2 #-}

-- instance (MToJSON m a, MToJSON m b) => MToJSON1 m ((,,) a b) where
--     mliftToEncoding = mliftToEncoding2 mtoEncoding mtoEncodingList
--     {-# INLINE mliftToEncoding #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c) => MToJSON m (a, b, c) where
--     mtoEncoding = mtoEncoding2
--     {-# INLINE mtoEncoding #-}

-- instance (MToJSON m a, MToJSON m b) => ToJSON2 ((,,,) a b) where
--     mliftToEncoding2 toC _ toD _ (a, b, c, d) = E.list id
--       [ mtoEncoding a
--       , mtoEncoding b
--       , toC c
--       , toD d
--       ]
--     {-# INLINE mliftToEncoding2 #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c) => MToJSON1 m ((,,,) a b c) where
--     mliftToEncoding = mliftToEncoding2 mtoEncoding mtoEncodingList
--     {-# INLINE mliftToEncoding #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d) => MToJSON m (a, b, c, d) where
--     mtoEncoding = mtoEncoding2
--     {-# INLINE mtoEncoding #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c) => ToJSON2 ((,,,,) a b c) where
--     mliftToEncoding2 toD _ toE _ (a, b, c, d, e) = E.list id
--       [ mtoEncoding a
--       , mtoEncoding b
--       , mtoEncoding c
--       , toD d
--       , toE e
--       ]
--     {-# INLINE mliftToEncoding2 #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d) => MToJSON1 m ((,,,,) a b c d) where
--     mliftToEncoding = mliftToEncoding2 mtoEncoding mtoEncodingList
--     {-# INLINE mliftToEncoding #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e) => MToJSON m (a, b, c, d, e) where
--     mtoEncoding = mtoEncoding2
--     {-# INLINE mtoEncoding #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d) => ToJSON2 ((,,,,,) a b c d) where
--     mliftToEncoding2 toE _ toF _ (a, b, c, d, e, f) = E.list id
--       [ mtoEncoding a
--       , mtoEncoding b
--       , mtoEncoding c
--       , mtoEncoding d
--       , toE e
--       , toF f
--       ]
--     {-# INLINE mliftToEncoding2 #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e) => MToJSON1 m ((,,,,,) a b c d e) where
--     mliftToEncoding = mliftToEncoding2 mtoEncoding mtoEncodingList
--     {-# INLINE mliftToEncoding #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f) => MToJSON m (a, b, c, d, e, f) where
--     mtoEncoding = mtoEncoding2
--     {-# INLINE mtoEncoding #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e) => ToJSON2 ((,,,,,,) a b c d e) where
--     mliftToEncoding2 toF _ toG _ (a, b, c, d, e, f, g) = E.list id
--         [ mtoEncoding a
--         , mtoEncoding b
--         , mtoEncoding c
--         , mtoEncoding d
--         , mtoEncoding e
--         , toF f
--         , toG g
--         ]
--     {-# INLINE mliftToEncoding2 #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f) => MToJSON1 m ((,,,,,,) a b c d e f) where
--     mliftToEncoding = mliftToEncoding2 mtoEncoding mtoEncodingList
--     {-# INLINE mliftToEncoding #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f, MToJSON m g) => MToJSON m (a, b, c, d, e, f, g) where
--     mtoEncoding = mtoEncoding2
--     {-# INLINE mtoEncoding #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f) => ToJSON2 ((,,,,,,,) a b c d e f) where
--     mliftToEncoding2 toG _ toH _ (a, b, c, d, e, f, g, h) = E.list id
--         [ mtoEncoding a
--         , mtoEncoding b
--         , mtoEncoding c
--         , mtoEncoding d
--         , mtoEncoding e
--         , mtoEncoding f
--         , toG g
--         , toH h
--         ]
--     {-# INLINE mliftToEncoding2 #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f, MToJSON m g) => MToJSON1 m ((,,,,,,,) a b c d e f g) where
--     mliftToEncoding = mliftToEncoding2 mtoEncoding mtoEncodingList
--     {-# INLINE mliftToEncoding #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f, MToJSON m g, MToJSON m h) => MToJSON m (a, b, c, d, e, f, g, h) where
--     mtoEncoding = mtoEncoding2
--     {-# INLINE mtoEncoding #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f, MToJSON m g) => ToJSON2 ((,,,,,,,,) a b c d e f g) where
--     mliftToEncoding2 toH _ toI _ (a, b, c, d, e, f, g, h, i) = E.list id
--         [ mtoEncoding a
--         , mtoEncoding b
--         , mtoEncoding c
--         , mtoEncoding d
--         , mtoEncoding e
--         , mtoEncoding f
--         , mtoEncoding g
--         , toH h
--         , toI i
--         ]
--     {-# INLINE mliftToEncoding2 #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f, MToJSON m g, MToJSON m h) => MToJSON1 m ((,,,,,,,,) a b c d e f g h) where
--     mliftToEncoding = mliftToEncoding2 mtoEncoding mtoEncodingList
--     {-# INLINE mliftToEncoding #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f, MToJSON m g, MToJSON m h, MToJSON m i) => MToJSON m (a, b, c, d, e, f, g, h, i) where
--     mtoEncoding = mtoEncoding2
--     {-# INLINE mtoEncoding #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f, MToJSON m g, MToJSON m h) => ToJSON2 ((,,,,,,,,,) a b c d e f g h) where
--     mliftToEncoding2 toI _ toJ _ (a, b, c, d, e, f, g, h, i, j) = E.list id
--         [ mtoEncoding a
--         , mtoEncoding b
--         , mtoEncoding c
--         , mtoEncoding d
--         , mtoEncoding e
--         , mtoEncoding f
--         , mtoEncoding g
--         , mtoEncoding h
--         , toI i
--         , toJ j
--         ]
--     {-# INLINE mliftToEncoding2 #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f, MToJSON m g, MToJSON m h, MToJSON m i) => MToJSON1 m ((,,,,,,,,,) a b c d e f g h i) where
--     mliftToEncoding = mliftToEncoding2 mtoEncoding mtoEncodingList
--     {-# INLINE mliftToEncoding #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f, MToJSON m g, MToJSON m h, MToJSON m i, MToJSON m j) => MToJSON m (a, b, c, d, e, f, g, h, i, j) where
--     mtoEncoding = mtoEncoding2
--     {-# INLINE mtoEncoding #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f, MToJSON m g, MToJSON m h, MToJSON m i) => ToJSON2 ((,,,,,,,,,,) a b c d e f g h i) where
--     mliftToEncoding2 toJ _ toK _ (a, b, c, d, e, f, g, h, i, j, k) = E.list id
--         [ mtoEncoding a
--         , mtoEncoding b
--         , mtoEncoding c
--         , mtoEncoding d
--         , mtoEncoding e
--         , mtoEncoding f
--         , mtoEncoding g
--         , mtoEncoding h
--         , mtoEncoding i
--         , toJ j
--         , toK k
--         ]
--     {-# INLINE mliftToEncoding2 #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f, MToJSON m g, MToJSON m h, MToJSON m i, MToJSON m j) => MToJSON1 m ((,,,,,,,,,,) a b c d e f g h i j) where
--     mliftToEncoding = mliftToEncoding2 mtoEncoding mtoEncodingList
--     {-# INLINE mliftToEncoding #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f, MToJSON m g, MToJSON m h, MToJSON m i, MToJSON m j, MToJSON m k) => MToJSON m (a, b, c, d, e, f, g, h, i, j, k) where
--     mtoEncoding = mtoEncoding2
--     {-# INLINE mtoEncoding #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f, MToJSON m g, MToJSON m h, MToJSON m i, MToJSON m j) => ToJSON2 ((,,,,,,,,,,,) a b c d e f g h i j) where
--     mliftToEncoding2 toK _ toL _ (a, b, c, d, e, f, g, h, i, j, k, l) = E.list id
--         [ mtoEncoding a
--         , mtoEncoding b
--         , mtoEncoding c
--         , mtoEncoding d
--         , mtoEncoding e
--         , mtoEncoding f
--         , mtoEncoding g
--         , mtoEncoding h
--         , mtoEncoding i
--         , mtoEncoding j
--         , toK k
--         , toL l
--         ]
--     {-# INLINE mliftToEncoding2 #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f, MToJSON m g, MToJSON m h, MToJSON m i, MToJSON m j, MToJSON m k) => MToJSON1 m ((,,,,,,,,,,,) a b c d e f g h i j k) where
--     mliftToEncoding = mliftToEncoding2 mtoEncoding mtoEncodingList
--     {-# INLINE mliftToEncoding #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f, MToJSON m g, MToJSON m h, MToJSON m i, MToJSON m j, MToJSON m k, MToJSON m l) => MToJSON m (a, b, c, d, e, f, g, h, i, j, k, l) where
--     mtoEncoding = mtoEncoding2
--     {-# INLINE mtoEncoding #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f, MToJSON m g, MToJSON m h, MToJSON m i, MToJSON m j, MToJSON m k) => ToJSON2 ((,,,,,,,,,,,,) a b c d e f g h i j k) where
--     mliftToEncoding2 toL _ toM _ (a, b, c, d, e, f, g, h, i, j, k, l, m) = E.list id
--         [ mtoEncoding a
--         , mtoEncoding b
--         , mtoEncoding c
--         , mtoEncoding d
--         , mtoEncoding e
--         , mtoEncoding f
--         , mtoEncoding g
--         , mtoEncoding h
--         , mtoEncoding i
--         , mtoEncoding j
--         , mtoEncoding k
--         , toL l
--         , toM m
--         ]
--     {-# INLINE mliftToEncoding2 #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f, MToJSON m g, MToJSON m h, MToJSON m i, MToJSON m j, MToJSON m k, MToJSON m l) => MToJSON1 m ((,,,,,,,,,,,,) a b c d e f g h i j k l) where
--     mliftToEncoding = mliftToEncoding2 mtoEncoding mtoEncodingList
--     {-# INLINE mliftToEncoding #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f, MToJSON m g, MToJSON m h, MToJSON m i, MToJSON m j, MToJSON m k, MToJSON m l, MToJSON m m) => MToJSON m (a, b, c, d, e, f, g, h, i, j, k, l, m) where
--     mtoEncoding = mtoEncoding2
--     {-# INLINE mtoEncoding #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f, MToJSON m g, MToJSON m h, MToJSON m i, MToJSON m j, MToJSON m k, MToJSON m l) => ToJSON2 ((,,,,,,,,,,,,,) a b c d e f g h i j k l) where
--     mliftToEncoding2 toM _ toN _ (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = E.list id
--         [ mtoEncoding a
--         , mtoEncoding b
--         , mtoEncoding c
--         , mtoEncoding d
--         , mtoEncoding e
--         , mtoEncoding f
--         , mtoEncoding g
--         , mtoEncoding h
--         , mtoEncoding i
--         , mtoEncoding j
--         , mtoEncoding k
--         , mtoEncoding l
--         , toM m
--         , toN n
--         ]
--     {-# INLINE mliftToEncoding2 #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f, MToJSON m g, MToJSON m h, MToJSON m i, MToJSON m j, MToJSON m k, MToJSON m l, MToJSON m m) => MToJSON1 m ((,,,,,,,,,,,,,) a b c d e f g h i j k l m) where
--     mliftToEncoding = mliftToEncoding2 mtoEncoding mtoEncodingList
--     {-# INLINE mliftToEncoding #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f, MToJSON m g, MToJSON m h, MToJSON m i, MToJSON m j, MToJSON m k, MToJSON m l, MToJSON m m, MToJSON m n) => MToJSON m (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
--     mtoEncoding = mtoEncoding2
--     {-# INLINE mtoEncoding #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f, MToJSON m g, MToJSON m h, MToJSON m i, MToJSON m j, MToJSON m k, MToJSON m l, MToJSON m m) => ToJSON2 ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m) where
--     mliftToEncoding2 toN _ toO _ (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = E.list id
--         [ mtoEncoding a
--         , mtoEncoding b
--         , mtoEncoding c
--         , mtoEncoding d
--         , mtoEncoding e
--         , mtoEncoding f
--         , mtoEncoding g
--         , mtoEncoding h
--         , mtoEncoding i
--         , mtoEncoding j
--         , mtoEncoding k
--         , mtoEncoding l
--         , mtoEncoding m
--         , toN n
--         , toO o
--         ]
--     {-# INLINE mliftToEncoding2 #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f, MToJSON m g, MToJSON m h, MToJSON m i, MToJSON m j, MToJSON m k, MToJSON m l, MToJSON m m, MToJSON m n) => MToJSON1 m ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m n) where
--     mliftToEncoding = mliftToEncoding2 mtoEncoding mtoEncodingList
--     {-# INLINE mliftToEncoding #-}

-- instance (MToJSON m a, MToJSON m b, MToJSON m c, MToJSON m d, MToJSON m e, MToJSON m f, MToJSON m g, MToJSON m h, MToJSON m i, MToJSON m j, MToJSON m k, MToJSON m l, MToJSON m m, MToJSON m n, MToJSON m o) => MToJSON m (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
--     mtoEncoding = mtoEncoding2
--     {-# INLINE mtoEncoding #-}






