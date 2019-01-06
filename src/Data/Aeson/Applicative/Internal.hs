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

module Data.Aeson.Applicative.Internal where

import Control.Applicative
import Control.Monad
import Data.Aeson
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
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Functor.Compose
import qualified Data.HashMap.Strict as H
import Data.List (intersperse)
import qualified Data.Map.Strict as M
import Data.Scientific
import Data.Semigroup as Semigroup
import Data.Tagged
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics
import Unsafe.Coerce

-- #############################################################################
-- From unexported Data/Aeson/Types/Generic.hs
-- #############################################################################

class IsRecord (f :: * -> *) (isRecord :: k) | f -> isRecord
  where
    isUnary :: f a -> Bool
    isUnary = const True

instance (IsRecord f isRecord) => IsRecord (f :*: g) isRecord
  where isUnary = const False
#if MIN_VERSION_base(4,9,0)
instance {-# OVERLAPPING #-} IsRecord (M1 S ('MetaSel 'Nothing u ss ds) f) 'False
#else
instance {-# OVERLAPPING #-} IsRecord (M1 S NoSelector f) 'False
#endif
instance (IsRecord f isRecord) => IsRecord (M1 S c f) isRecord
instance IsRecord (K1 i c) 'True
instance IsRecord Par1 'True
instance IsRecord (Rec1 f) 'True
instance IsRecord (f :.: g) 'True
instance IsRecord U1 'False
  where isUnary = const False

--------------------------------------------------------------------------------

class AllNullary (f :: * -> *) allNullary | f -> allNullary where

instance ( AllNullary a allNullaryL
        , AllNullary b allNullaryR
        , And allNullaryL allNullaryR allNullary
        ) => AllNullary (a :+: b) allNullary
instance AllNullary a allNullary => AllNullary (M1 i c a) allNullary
instance AllNullary (a :*: b) 'False
instance AllNullary (a :.: b) 'False
instance AllNullary (K1 i c) 'False
instance AllNullary Par1 'False
instance AllNullary (Rec1 f) 'False
instance AllNullary U1 'True

--------------------------------------------------------------------------------

class And bool1 bool2 bool3 | bool1 bool2 -> bool3 where

instance And 'True  'True  'True
instance And 'False 'False 'False
instance And 'False 'True  'False
instance And 'True  'False 'False


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
    => (k -> Encoding' T.Text)                     -- ^ key encoding
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
  fromString = String . T.pack

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
#if MIN_VERSION_aeson(1,3,0)
#else
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
    pair k v = DList.singleton (T.pack k .= v)

instance (e ~ Encoding) => KeyValuePair e Series where
    pair = pairStr
      where
        pairStr :: String -> Encoding -> Series
        pairStr name val = pair' (string name) val
        {-# INLINE pairStr #-}

        pair' :: Encoding' T.Text -> Encoding -> Series
        pair' name val = Value $ retagEncoding $ retagEncoding name >< colon >< val

#endif


-- #############################################################################
-- Data.Aeson.Types.FromJSON
-- #############################################################################

-- | Annotate parsing array with Index of parsing error
_parseIndexedJSON :: (Value -> Parser a) -> Int -> Value -> Parser a
_parseIndexedJSON p idx value = p value <?> Index idx

-- | modified to use a combining function
_parseIndexedJSONPair :: (a -> b -> c) -> (Value -> Parser a) -> (Value -> Parser b) -> Int -> Value -> Parser c
_parseIndexedJSONPair f keyParser valParser idx value = p value <?> Index idx
  where
    p = withArray "(k,v)" $ \ab ->
        let n = V.length ab
        in if n == 2
             then f <$> _parseJSONElemAtIndex keyParser 0 ab
                      <*> _parseJSONElemAtIndex valParser 1 ab
             else fail $ "cannot unpack array of length " ++
                         show n ++ " into a pair"

-- | duplicated because it was not exported
_parseJSONElemAtIndex :: (Value -> Parser a) -> Int -> V.Vector Value -> Parser a
_parseJSONElemAtIndex p idx ary = p (V.unsafeIndex ary idx) <?> Index idx

-------------------------------------------------------------------------------
-- Generics
-------------------------------------------------------------------------------

-- | Class of generic representation types that can be converted from JSON.
class Applicative m => MGFromJSON m arity f where
    -- | This method (applied to 'defaultOptions') is used as the
    -- default generic implementation of 'parseJSON' (if the @arity@ is 'Zero')
    -- or 'liftParseJSON' (if the @arity@ is 'One').
    mgParseJSON :: Options -> MFromArgs m arity a -> Value -> Parser (m (f a))

-- | A 'FromArgs' value either stores nothing (for 'FromJSON') or it stores the
-- two function arguments that decode occurrences of the type parameter (for
-- 'FromJSON1').
data MFromArgs m arity a where
    MNoFromArgs :: MFromArgs m Zero a
    MFrom1Args  :: (Value -> Parser (m a)) -> (Value -> Parser (m [a])) -> MFromArgs m One a

-- | A configurable generic JSON decoder. This function applied to
-- 'defaultOptions' is used as the default for 'parseJSON' when the
-- type is an instance of 'Generic'.
mgenericParseJSON :: (Generic a, MGFromJSON m Zero (Rep a))
                 => Options -> Value -> Parser (m a)
mgenericParseJSON opts = fmap (fmap to) . mgParseJSON opts MNoFromArgs

-- | A configurable generic JSON decoder. This function applied to
-- 'defaultOptions' is used as the default for 'liftParseJSON' when the
-- type is an instance of 'Generic1'.
mgenericLiftParseJSON :: (Generic1 f, MGFromJSON m One (Rep1 f))
                     => Options -> (Value -> Parser (m a)) -> (Value -> Parser (m [a]))
                     -> Value -> Parser (m (f a))
mgenericLiftParseJSON opts pj pjl = fmap (fmap to1) . mgParseJSON opts (MFrom1Args pj pjl)

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

class Applicative m => MFromJSON m a where
    mparseJSON :: Value -> Parser (m a)

    default mparseJSON :: (Generic a, MGFromJSON m Zero (Rep a)) => Value -> Parser (m a)
    mparseJSON = mgenericParseJSON defaultOptions

    mparseJSONList :: Value -> Parser (m [a])
    mparseJSONList (Array a)
        = fmap sequenceA
        . zipWithM (_parseIndexedJSON mparseJSON) [0..]
        . V.toList
        $ a

    mparseJSONList v = typeMismatch "[a]" v

-------------------------------------------------------------------------------
-- Lifings of FromJSON and ToJSON to unary and binary type constructors
-------------------------------------------------------------------------------

-- | Analogous to 'fromJSON1'
class Applicative m => MFromJSON1 m f where
    mliftParseJSON :: (Value -> Parser (m a)) -> (Value -> Parser (m [a])) -> Value -> Parser (m (f a))

    default mliftParseJSON :: (Generic1 f, MGFromJSON m One (Rep1 f))
                          => (Value -> Parser (m a)) -> (Value -> Parser (m [a])) -> Value -> Parser (m (f a))
    mliftParseJSON = mgenericLiftParseJSON defaultOptions

    -- listParser :: (Value -> Parser a) -> Value -> Parser [a]
    mliftParseJSONList :: (Value -> Parser (m a)) -> (Value -> Parser (m [a])) -> Value -> Parser (m [f a])
    mliftParseJSONList f g v = sequenceA <$> listParser (mliftParseJSON f g) v

-- | Analogous to 'parseJSON1'
-- Lift the standard 'parseJSON' function through the type constructor.
mparseJSON1 :: (MFromJSON1 m f, MFromJSON m a) => Value -> Parser (m (f a))
mparseJSON1 = mliftParseJSON mparseJSON mparseJSONList

-- -------------------------------------------------------------------------------
-- -- List functions
-- -------------------------------------------------------------------------------

-- | Helper function to use with 'liftParseJSON'. See 'Data.Aeson.ToJSON.listEncoding'.
mlistParser :: Applicative m => (Value -> Parser (m a)) -> Value -> Parser (m [a])
mlistParser f (Array xs) = fmap (sequenceA . V.toList) (V.mapM f xs)
mlistParser _ v = typeMismatch "[a]" v
{-# INLINE mlistParser #-}

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'empty' if the key is not present or the value cannot
-- be converted to the desired type.
--
-- This accessor is appropriate if the key and value /must/ be present
-- in an object for it to be valid.  If the key and value are
-- optional, use '.:?' instead.
--
-- based on (.:)
retrieve :: (MFromJSON m a) => Object -> T.Text -> Parser (m a)
retrieve = mexplicitParseField mparseJSON
{-# INLINE retrieve #-}

-- | Retrieve the value associated with the given key of an 'Object'. The
-- result is 'Nothing' if the key is not present or if its value is 'Null',
-- or 'empty' if the value cannot be converted to the desired type.
--
-- This accessor is most useful if the key and value can be absent
-- from an object without affecting its validity.  If the key and
-- value are mandatory, use '.:' instead.
-- based on (.:?)
maybeRetrieve :: (Applicative m, MFromJSON m a) => Object -> T.Text -> Parser (m (Maybe a))
maybeRetrieve = mexplicitParseFieldMaybe mparseJSON
{-# INLINE maybeRetrieve #-}

-- | Variant of '.:' with explicit parser function.
--
-- g. @'explicitParseField' 'parseJSON1' :: ('FromJSON1' f, 'FromJSON' a) -> 'Object' -> 'Text' -> 'Parser' (f a)@
mexplicitParseField :: (Value -> Parser (m a)) -> Object -> T.Text -> Parser (m a)
mexplicitParseField p obj key = case H.lookup key obj of
    Nothing -> fail $ "key " ++ show key ++ " not present"
    Just v  -> p v <?> Key key
{-# INLINE mexplicitParseField #-}

-- | Variant of '.:?' with explicit parser function.
mexplicitParseFieldMaybe :: Applicative m => (Value -> Parser (m a)) -> Object -> T.Text -> Parser (m (Maybe a))
mexplicitParseFieldMaybe p obj key = case H.lookup key obj of
    Nothing -> pure (pure Nothing)
    Just v  -> mliftParseJSON p (mlistParser p) v <?> Key key -- listParser isn't used by maybe instance.
{-# INLINE mexplicitParseFieldMaybe #-}

--------------------------------------------------------------------------------
-- Generic parseJSON
-------------------------------------------------------------------------------

instance Applicative m => MGFromJSON m arity V1 where
    -- Whereof we cannot format, thereof we cannot parse:
    mgParseJSON _ _ _ = fail "Attempted to parse empty type"

instance {-# OVERLAPPABLE #-} (MGFromJSON m arity a) => MGFromJSON m arity (M1 i c a) where
    -- Meta-information, which is not handled elsewhere, is just added to the
    -- parsed value:
    mgParseJSON opts fargs = fmap (fmap M1) . mgParseJSON opts fargs

instance (MFromJSON m a) => MGFromJSON m arity (K1 i a) where
    -- Constant values are decoded using their FromJSON instance:
    mgParseJSON _opts _ = fmap (fmap K1) . mparseJSON

instance Applicative m => MGFromJSON m One Par1 where
    -- Direct occurrences of the last type parameter are decoded with the
    -- function passed in as an argument:
    mgParseJSON _opts (MFrom1Args pj _) = fmap (fmap Par1) . pj

instance (Applicative m, MFromJSON1 m f) => MGFromJSON m One (Rec1 f) where
    -- Recursive occurrences of the last type parameter are decoded using their
    -- FromJSON1 instance:
    mgParseJSON _opts (MFrom1Args pj pjl) = fmap (fmap Rec1) . mliftParseJSON pj pjl

instance Applicative m => MGFromJSON m arity U1 where
    -- Empty constructors are expected to be encoded as an empty array:
    mgParseJSON _opts _ v
        | _isEmptyArray v = pure (pure U1)
        | otherwise      = typeMismatch "unit constructor (U1)" v
      where
        -- | Determines if the 'Value' is an empty 'Array'.
        -- Note that: @isEmptyArray 'emptyArray'@.
        _isEmptyArray :: Value -> Bool
        _isEmptyArray (Array arr) = V.null arr
        _isEmptyArray _ = False

-- instance ( MConsFromJSON m arity a
--          , AllNullary (C1 c a) allNullary
--          , MParseSum m arity (C1 c a) allNullary
--          ) => GFromJSON m arity (D1 d (C1 c a)) where
--     -- The option 'tagSingleConstructors' determines whether to wrap
--     -- a single-constructor type.
--     mgParseJSON opts fargs
--         | tagSingleConstructors opts
--             = fmap M1
--             . (unTagged :: Tagged allNullary (Parser (C1 c a p)) -> Parser (C1 c a p))
--             . parseSum opts fargs
--         | otherwise = fmap M1 . fmap M1 . consParseJSON opts fargs

-- instance (ConsFromJSON arity a) => GFromJSON arity (C1 c a) where
--     -- Constructors need to be decoded differently depending on whether they're
--     -- a record or not. This distinction is made by consParseJSON:
--     gParseJSON opts fargs = fmap M1 . consParseJSON opts fargs

-- instance ( FromProduct arity a, FromProduct arity b
--          , ProductSize       a, ProductSize       b
--          ) => GFromJSON arity (a :*: b) where
--     -- Products are expected to be encoded to an array. Here we check whether we
--     -- got an array of the same size as the product, then parse each of the
--     -- product's elements using parseProduct:
--     gParseJSON opts fargs = withArray "product (:*:)" $ \arr ->
--       let lenArray = V.length arr
--           lenProduct = (unTagged2 :: Tagged2 (a :*: b) Int -> Int)
--                        productSize in
--       if lenArray == lenProduct
--       then parseProduct opts fargs arr 0 lenProduct
--       else fail $ "When expecting a product of " ++ show lenProduct ++
--                   " values, encountered an Array of " ++ show lenArray ++
--                   " elements instead"

-- instance ( AllNullary         (a :+: b) allNullary
--          , ParseSum     arity (a :+: b) allNullary
--          ) => GFromJSON arity (a :+: b) where
--     -- If all constructors of a sum datatype are nullary and the
--     -- 'allNullaryToStringTag' option is set they are expected to be
--     -- encoded as strings.  This distinction is made by 'parseSum':
--     gParseJSON opts fargs =
--       (unTagged :: Tagged allNullary (Parser ((a :+: b) d)) ->
--                                      Parser ((a :+: b) d))
--                  . parseSum opts fargs

-- instance (FromJSON1 f, GFromJSON One g) => GFromJSON One (f :.: g) where
--     -- If an occurrence of the last type parameter is nested inside two
--     -- composed types, it is decoded by using the outermost type's FromJSON1
--     -- instance to generically decode the innermost type:
--     gParseJSON opts fargs =
--       let gpj = gParseJSON opts fargs in
--       fmap Comp1 . liftParseJSON gpj (listParser gpj)

--------------------------------------------------------------------------------

class MParseSum m arity f allNullary where
    mparseSum :: Options -> MFromArgs m arity a
             -> Value -> Tagged allNullary (Parser (m (f a)))

-- instance ( MSumFromString m f
--          , MFromPair m arity f
--          , FromTaggedObject  arity f
--          , FromUntaggedValue arity f
--          ) => ParseSum       arity f True where
--     parseSum opts fargs
--         | allNullaryToStringTag opts = Tagged . parseAllNullarySum    opts
--         | otherwise                  = Tagged . parseNonAllNullarySum opts fargs

-- instance ( FromPair          arity f
--          , FromTaggedObject  arity f
--          , FromUntaggedValue arity f
--          ) => ParseSum       arity f False where
--     parseSum opts fargs = Tagged . parseNonAllNullarySum opts fargs

--------------------------------------------------------------------------------

mparseAllNullarySum :: MSumFromString m f => Options -> Value -> Parser (m (f a))
mparseAllNullarySum opts = withText "Text" $ \key ->
                            maybe (notFound key) pure $
                              mparseSumFromString opts key

class MSumFromString m f where
    mparseSumFromString :: Options -> T.Text -> Maybe (m (f a))

instance (Functor m, MSumFromString m a, MSumFromString m b) => MSumFromString m (a :+: b) where
    mparseSumFromString opts key = (fmap L1 <$> mparseSumFromString opts key) <|>
                                  (fmap R1 <$> mparseSumFromString opts key)

instance (Applicative m, Constructor c) => MSumFromString m (C1 c U1) where
    mparseSumFromString opts key | key == name = Just $ pure $ M1 U1
                                 | otherwise   = Nothing
        where
          name = T.pack $ constructorTagModifier opts $
                          conName (undefined :: t c U1 p)

--------------------------------------------------------------------------------

-- parseNonAllNullarySum :: ( FromPair          arity f
--                          , FromTaggedObject  arity f
--                          , FromUntaggedValue arity f
--                          ) => Options -> FromArgs arity c
--                            -> Value -> Parser (f c)
-- parseNonAllNullarySum opts fargs =
--     case sumEncoding opts of
--       TaggedObject{..} ->
--           withObject "Object" $ \obj -> do
--             tag <- obj .: pack tagFieldName
--             fromMaybe (notFound tag) $
--               parseFromTaggedObject opts fargs contentsFieldName obj tag

--       ObjectWithSingleField ->
--           withObject "Object" $ \obj ->
--             case H.toList obj of
--               [pair@(tag, _)] -> fromMaybe (notFound tag) $
--                                    parsePair opts fargs pair
--               _ -> fail "Object doesn't have a single field"

--       TwoElemArray ->
--           withArray "Array" $ \arr ->
--             if V.length arr == 2
--             then case V.unsafeIndex arr 0 of
--                    String tag -> fromMaybe (notFound tag) $
--                                    parsePair opts fargs (tag, V.unsafeIndex arr 1)
--                    _ -> fail "First element is not a String"
--             else fail "Array doesn't have 2 elements"

--       UntaggedValue -> parseUntaggedValue opts fargs

--------------------------------------------------------------------------------

class MFromTaggedObject m arity f where
    mparseFromTaggedObject :: Options -> MFromArgs m arity a
                          -> String -> Object
                          -> T.Text -> Maybe (Parser (m (f a)))

instance ( Functor m, MFromTaggedObject m arity a, MFromTaggedObject m arity b) =>
    MFromTaggedObject m arity (a :+: b) where
        mparseFromTaggedObject opts fargs contentsFieldName obj tag =
            (fmap (fmap L1) <$> mparseFromTaggedObject opts fargs contentsFieldName obj tag) <|>
            (fmap (fmap R1) <$> mparseFromTaggedObject opts fargs contentsFieldName obj tag)

instance ( Functor m, MFromTaggedObject' m arity f
         , Constructor c
         ) => MFromTaggedObject m arity (C1 c f) where
    mparseFromTaggedObject opts fargs contentsFieldName obj tag
        | tag == name = Just $ (fmap M1) <$> mparseFromTaggedObject'
                                        opts fargs contentsFieldName obj
        | otherwise = Nothing
        where
          name = T.pack $ constructorTagModifier opts $
                          conName (undefined :: t c f p)

--------------------------------------------------------------------------------

class MFromTaggedObject' m arity f where
    mparseFromTaggedObject' :: Options -> MFromArgs m arity a -> String
                           -> Object -> Parser (m (f a))

class MFromTaggedObject'' m arity f isRecord where
    mparseFromTaggedObject'' :: Options -> MFromArgs m arity a -> String
                            -> Object -> Tagged isRecord (Parser (m (f a)))

instance ( IsRecord f isRecord
         , MFromTaggedObject'' m arity f isRecord
         ) => MFromTaggedObject' m arity f where
    mparseFromTaggedObject' opts fargs contentsFieldName =
        (unTagged :: Tagged isRecord (Parser (m (f a))) -> Parser (m (f a))) .
        mparseFromTaggedObject'' opts fargs contentsFieldName

-- instance (MFromRecord m arity f) => MFromTaggedObject'' m arity f 'True where
--     mparseFromTaggedObject'' opts fargs _ =
--       Tagged . mparseRecord opts fargs

-- instance (MGFromJSON arity f) => MFromTaggedObject'' m arity f 'False where
--     parseFromTaggedObject'' opts fargs contentsFieldName = Tagged .
--       (mgParseJSON opts fargs <=< (.: pack contentsFieldName))

instance {-# OVERLAPPING #-} Applicative m => MFromTaggedObject'' m arity U1 'False where
    mparseFromTaggedObject'' _ _ _ _ = Tagged (pure (pure U1))

--------------------------------------------------------------------------------

class MConsFromJSON m arity f where
    mconsParseJSON  :: Options -> MFromArgs m arity a
                   -> Value -> Parser (m (f a))

class MConsFromJSON' m arity f isRecord where
    mconsParseJSON' :: Options -> MFromArgs m arity a
                   -> Value -> Tagged isRecord (Parser (m (f a)))

instance ( IsRecord f isRecord
         , MConsFromJSON' m arity f isRecord
         ) => MConsFromJSON m arity f where
    mconsParseJSON opts fargs =
      (unTagged :: Tagged isRecord (Parser (m (f a))) -> Parser (m (f a)))
        . mconsParseJSON' opts fargs

instance {-# OVERLAPPING #-}
         ( MGFromJSON m arity a, MFromRecord m arity (S1 s a)
         ) => MConsFromJSON' m arity (S1 s a) 'True where
    mconsParseJSON' opts fargs
      | unwrapUnaryRecords opts = Tagged . mgParseJSON opts fargs
      | otherwise = Tagged . withObject "unary record" (mparseRecord opts fargs)

-- instance MFromRecord m arity f => ConsFromJSON' m arity f 'True where
--     mconsParseJSON' opts fargs =
--       Tagged . withObject "record (:*:)" (parseRecord opts fargs)

instance MGFromJSON m arity f => MConsFromJSON' m arity f 'False where
    mconsParseJSON' opts fargs = Tagged . mgParseJSON opts fargs

--------------------------------------------------------------------------------

class MFromRecord m arity f where
    mparseRecord :: Options -> MFromArgs m arity a
                -> Object -> Parser (m (f a))

instance
    ( Applicative m
    , MFromRecord m arity a
    , MFromRecord m arity b
    ) => MFromRecord m arity (a :*: b) where
    mparseRecord opts fargs obj = getCompose $
      (:*:) <$> Compose (mparseRecord opts fargs obj)
            <*> Compose (mparseRecord opts fargs obj)

instance {-# OVERLAPPABLE #-} (Selector s, MGFromJSON m arity a) =>
  MFromRecord m arity (S1 s a) where
    mparseRecord opts fargs =
      (<?> Key label) . mgParseJSON opts fargs <=< (.: label)
        where
          label = T.pack . fieldLabelModifier opts $ selName (undefined :: t s a p)

instance {-# INCOHERENT #-} (Selector s, MFromJSON m a) =>
  MFromRecord m arity (S1 s (K1 i (Maybe a))) where
    mparseRecord opts _ obj = fmap (M1 . K1) <$> obj `maybeRetrieve` T.pack label
        where
          label = fieldLabelModifier opts $
                    selName (undefined :: t s (K1 i (Maybe a)) p)

-- Parse an Option like a Maybe.
instance {-# INCOHERENT #-} (Selector s, MFromJSON m a) =>
  MFromRecord m arity (S1 s (K1 i (Semigroup.Option a))) where
    mparseRecord opts fargs obj = fmap wrap <$> mparseRecord opts fargs obj
      where
        wrap :: S1 s (K1 i (Maybe a)) p -> S1 s (K1 i (Semigroup.Option a)) p
        wrap (M1 (K1 a)) = M1 (K1 (Semigroup.Option a))

--------------------------------------------------------------------------------

-- class FromProduct arity f where
--     parseProduct :: Options -> FromArgs arity a
--                  -> Array -> Int -> Int
--                  -> Parser (f a)

-- instance ( FromProduct    arity a
--          , FromProduct    arity b
--          ) => FromProduct arity (a :*: b) where
--     parseProduct opts fargs arr ix len =
--         (:*:) <$> parseProduct opts fargs arr ix  lenL
--               <*> parseProduct opts fargs arr ixR lenR
--         where
--           lenL = len `unsafeShiftR` 1
--           ixR  = ix + lenL
--           lenR = len - lenL

-- instance (GFromJSON arity a) => FromProduct arity (S1 s a) where
--     parseProduct opts fargs arr ix _ =
--       gParseJSON opts fargs $ V.unsafeIndex arr ix

--------------------------------------------------------------------------------

class MFromPair m arity f where
    mparsePair :: Options -> MFromArgs m arity a
              -> Pair -> Maybe (Parser (m (f a)))

instance
    ( Functor m
    , MFromPair m arity a
    , MFromPair m arity b
    ) => MFromPair m arity (a :+: b) where
    mparsePair opts fargs pr = (fmap (fmap L1) <$> mparsePair opts fargs pr) <|>
                                (fmap (fmap R1) <$> mparsePair opts fargs pr)

-- instance ( Constructor c
--          , MGFromJSON m arity a
--          , MConsFromJSON m arity a
--          ) => MFromPair m arity (C1 c a) where
--     mparsePair opts fargs (tag, value)
--         | tag == tag' = Just $ mgParseJSON opts fargs value
--         | otherwise   = Nothing
--         where
--           tag' = T.pack $ constructorTagModifier opts $
--                           conName (undefined :: t c a p)

-- --------------------------------------------------------------------------------

-- class FromUntaggedValue arity f where
--     parseUntaggedValue :: Options -> FromArgs arity a
--                        -> Value -> Parser (f a)

-- instance
--     ( FromUntaggedValue    arity a
--     , FromUntaggedValue    arity b
--     ) => FromUntaggedValue arity (a :+: b)
--   where
--     parseUntaggedValue opts fargs value =
--         L1 <$> parseUntaggedValue opts fargs value <|>
--         R1 <$> parseUntaggedValue opts fargs value

-- instance {-# OVERLAPPABLE #-}
--     ( GFromJSON            arity a
--     , ConsFromJSON         arity a
--     ) => FromUntaggedValue arity (C1 c a)
--   where
--     parseUntaggedValue = gParseJSON

-- instance {-# OVERLAPPING #-}
--     ( Constructor c )
--     => FromUntaggedValue arity (C1 c U1)
--   where
--     parseUntaggedValue opts _ (String s)
--         | s == pack (constructorTagModifier opts (conName (undefined :: t c U1 p))) =
--             pure $ M1 U1
--         | otherwise =
--             fail $ "Invalid tag: " ++ unpack s
--     parseUntaggedValue _ _ v = typeMismatch (conName (undefined :: t c U1 p)) v

--------------------------------------------------------------------------------

notFound :: T.Text -> Parser a
notFound key = fail $ "The key \"" ++ T.unpack key ++ "\" was not found"
{-# INLINE notFound #-}

-- --------------------------------------------------------------------------------


-- #############################################################################
-- OLD
-- #############################################################################

-- -----------------------------------------------------
-- MToJSON1, MFromJSON1 Instances
-- -----------------------------------------------------

instance Applicative m => MToJSON1 m [] where
    mliftToEncoding _ p' = p'
    {-# INLINE mliftToEncoding #-}

instance Applicative m => MFromJSON1 m [] where
    mliftParseJSON _ p' = p'
    {-# INLINE mliftParseJSON #-}

instance Applicative m => MToJSON1 m Maybe where
    mliftToEncoding t _ (Just a) = t a
    mliftToEncoding _  _ Nothing  = pure null_

instance Applicative m => MFromJSON1 m Maybe where
    mliftParseJSON _ _ Null = pure (pure Nothing)
    mliftParseJSON p _ a    = fmap Just <$> p a

instance (Applicative m, ToJSONKey k) => MToJSON1 m (M.Map k) where
    mliftToEncoding g _ = case toJSONKey of
        ToJSONKeyText _ f -> mdictEncoding f g M.foldrWithKey
        ToJSONKeyValue _ f -> mlistEncoding (pairEncoding f) . M.toList
      where
        pairEncoding f (a, b) = mlistEncoding id [pure $ f a, g b]

instance (Applicative m, FromJSONKey k, Ord k) => MFromJSON1 m (M.Map k) where
    mliftParseJSON p _ = case fromJSONKey of
        FromJSONKeyCoerce _ -> withObject "Map k v" $
            H.foldrWithKey (\k v m -> getCompose $ M.insert
                <$> (Compose $ (pure . pure $ unsafeCoerce k) <?> Key k)
                <*> (Compose $ p v <?> Key k)
                <*> Compose m)
                (pure $ pure M.empty)

        FromJSONKeyText f -> withObject "Map k v" $
            H.foldrWithKey (\k v m -> getCompose $ M.insert
                <$> (Compose $ (pure . pure $ f k) <?> Key k)
                <*> (Compose $ p v <?> Key k)
                <*> Compose m)
                (pure $ pure M.empty)

        FromJSONKeyTextParser f -> withObject "Map k v" $
            H.foldrWithKey (\k v m -> getCompose $ M.insert
                <$> (Compose $ (pure <$> f k) <?> Key k)
                <*> (Compose $ p v <?> Key k)
                <*> Compose m)
                (pure $ pure M.empty)

        FromJSONKeyValue f -> withArray "Map k v" $ \arr ->
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
    mtoEncoding = pure . toEncoding

instance Applicative m => MFromJSON m Bool where
    mparseJSON = fmap pure . parseJSON

instance Applicative m => MToJSON m Char where
    mtoEncoding = pure . toEncoding

instance Applicative m => MFromJSON m Char where
    mparseJSON = fmap pure . parseJSON

instance Applicative m => MToJSON m T.Text where
    mtoEncoding = pure . toEncoding

instance Applicative m => MFromJSON m T.Text where
    mparseJSON = fmap pure . parseJSON

instance Applicative m => MToJSON m Scientific where
    mtoEncoding = pure . toEncoding

instance Applicative m => MFromJSON m Scientific where
    mparseJSON = fmap pure . parseJSON

instance Applicative m => MToJSON m Value where
    mtoEncoding = pure . toEncoding

instance Applicative m => MFromJSON m Value where
    mparseJSON = fmap pure . parseJSON

instance Applicative m => MToJSON m Int where
    mtoEncoding = pure . toEncoding

instance Applicative m => MFromJSON m Int where
    mparseJSON = fmap pure . parseJSON

instance Applicative m => MToJSON m Array where
    mtoEncoding = pure . toEncoding

instance Applicative m => MToJSON m Object where
    mtoEncoding = pure . toEncoding

instance MToJSON m a => MToJSON m [a] where
    {-# SPECIALIZE instance Applicative m => MToJSON m String #-}
    {-# SPECIALIZE instance Applicative m => MToJSON m [String] #-}
    {-# SPECIALIZE instance Applicative m => MToJSON m [Array] #-}
    {-# SPECIALIZE instance Applicative m => MToJSON m [Object] #-}

    mtoEncoding = mtoEncoding1
    {-# INLINE mtoEncoding #-}

instance MFromJSON m a => MFromJSON m [a] where
    mparseJSON = mparseJSON1

instance (MToJSON m a, ToJSONKey k) => MToJSON m (M.Map k a) where
    mtoEncoding = mtoEncoding1

instance (MFromJSON m a, FromJSONKey k, Ord k) => MFromJSON m (M.Map k a) where
    mparseJSON = mparseJSON1

instance (MToJSON m a) => MToJSON m (Maybe a) where
    mtoEncoding = mtoEncoding1

instance (MFromJSON m a) => MFromJSON m (Maybe a) where
    mparseJSON = mparseJSON1

