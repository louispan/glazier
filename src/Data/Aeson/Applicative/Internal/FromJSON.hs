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
{-# LANGUAGE ViewPatterns #-}

module Data.Aeson.Applicative.Internal.FromJSON where

import Control.Applicative (Const(..), (<|>))
import Control.Monad (zipWithM, (<=<))
import Data.Aeson
import Data.Aeson.Applicative.Internal.Generic
import Data.Aeson.Internal
import Data.Aeson.Types
import Data.Bits (unsafeShiftR)
import qualified Data.DList as DList
import Data.Fixed (Fixed, HasResolution)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(..))
import Data.Functor.Sum (Sum(..))
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HashSet
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Monoid as Monoid
import Data.Proxy (Proxy(..))
import Data.Ratio (Ratio)
import Data.Scientific (Scientific, base10Exponent)
import qualified Data.Semigroup as Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Tagged (Tagged(..))
import Data.Text (Text, pack, unpack)
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
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types (CTime(..))
import Foreign.Storable (Storable)
import GHC.Generics
import Numeric.Natural (Natural)
import Unsafe.Coerce (unsafeCoerce)

#if MIN_VERSION_primitive(0,6,4)
import qualified GHC.Exts as Exts
import qualified Data.Primitive.PrimArray as PM
import qualified Data.Primitive.SmallArray as PM
import qualified Data.Primitive.UnliftedArray as PM
#endif

-- Based on aeson-1.4.2.0

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
class Applicative m => AGFromJSON m arity f where
    -- | This method (applied to 'defaultOptions') is used as the
    -- default generic implementation of 'parseJSON' (if the @arity@ is 'Zero')
    -- or 'liftParseJSON' (if the @arity@ is 'One').
    agParseJSON :: Options -> AFromArgs m arity a -> Value -> Parser (m (f a))

-- | A 'FromArgs' value either stores nothing (for 'FromJSON') or it stores the
-- two function arguments that decode occurrences of the type parameter (for
-- 'FromJSON1').
data AFromArgs m arity a where
    ANoFromArgs :: AFromArgs m Zero a
    AFrom1Args  :: (Value -> Parser (m a)) -> (Value -> Parser (m [a])) -> AFromArgs m One a

-- | A configurable generic JSON decoder. This function applied to
-- 'defaultOptions' is used as the default for 'parseJSON' when the
-- type is an instance of 'Generic'.
agenericParseJSON :: (Generic a, AGFromJSON m Zero (Rep a))
                 => Options -> Value -> Parser (m a)
agenericParseJSON opts = fmap (fmap to) . agParseJSON opts ANoFromArgs

-- | A configurable generic JSON decoder. This function applied to
-- 'defaultOptions' is used as the default for 'liftParseJSON' when the
-- type is an instance of 'Generic1'.
agenericLiftParseJSON :: (Generic1 f, AGFromJSON m One (Rep1 f))
                     => Options -> (Value -> Parser (m a)) -> (Value -> Parser (m [a]))
                     -> Value -> Parser (m (f a))
agenericLiftParseJSON opts pj pjl = fmap (fmap to1) . agParseJSON opts (AFrom1Args pj pjl)

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

class Applicative m => AFromJSON m a where
    aparseJSON :: Value -> Parser (m a)

    default aparseJSON :: (Generic a, AGFromJSON m Zero (Rep a)) => Value -> Parser (m a)
    aparseJSON = agenericParseJSON defaultOptions

    aparseJSONList :: Value -> Parser (m [a])
    aparseJSONList (Array a)
        = fmap sequenceA
        . zipWithM (_parseIndexedJSON aparseJSON) [0..]
        . V.toList
        $ a

    aparseJSONList v = typeMismatch "[a]" v

-------------------------------------------------------------------------------
-- Lifings of FromJSON and ToJSON to unary and binary type constructors
-------------------------------------------------------------------------------

-- | Analogous to 'fromJSON1'
class Applicative m => AFromJSON1 m f where
    aliftParseJSON :: (Value -> Parser (m a)) -> (Value -> Parser (m [a])) -> Value -> Parser (m (f a))

    default aliftParseJSON :: (Generic1 f, AGFromJSON m One (Rep1 f))
                          => (Value -> Parser (m a)) -> (Value -> Parser (m [a])) -> Value -> Parser (m (f a))
    aliftParseJSON = agenericLiftParseJSON defaultOptions

    -- listParser :: (Value -> Parser a) -> Value -> Parser [a]
    aliftParseJSONList :: (Value -> Parser (m a)) -> (Value -> Parser (m [a])) -> Value -> Parser (m [f a])
    aliftParseJSONList f g v = sequenceA <$> listParser (aliftParseJSON f g) v

-- | Analogous to 'parseJSON1'
-- Lift the standard 'parseJSON' function through the type constructor.
aparseJSON1 :: (AFromJSON1 m f, AFromJSON m a) => Value -> Parser (m (f a))
aparseJSON1 = aliftParseJSON aparseJSON aparseJSONList

-- | Lifting of the 'FromJSON' class to binary type constructors.
--
-- Instead of manually writing your 'FromJSON2' instance, "Data.Aeson.TH"
-- provides Template Haskell functions which will derive an instance at compile time.

-- The compiler cannot provide a default generic implementation for 'liftParseJSON2',
-- unlike 'parseJSON' and 'liftParseJSON'.
class Applicative m => AFromJSON2 m f where
    aliftParseJSON2
        :: (Value -> Parser (m a))
        -> (Value -> Parser (m [a]))
        -> (Value -> Parser (m b))
        -> (Value -> Parser (m [b]))
        -> Value -> Parser (m (f a b))
    aliftParseJSONList2
        :: (Value -> Parser (m a))
        -> (Value -> Parser (m [a]))
        -> (Value -> Parser (m b))
        -> (Value -> Parser (m [b]))
        -> Value -> Parser (m [f a b])
    aliftParseJSONList2 fa ga fb gb v = case v of
        Array vals -> fmap (fmap V.toList . sequenceA) $ V.mapM (aliftParseJSON2 fa ga fb gb) vals
        _ -> typeMismatch "[a]" v

-- | Lift the standard 'parseJSON' function through the type constructor.
aparseJSON2 :: (AFromJSON2 m f, AFromJSON m a, AFromJSON m b) => Value -> Parser (m (f a b))
aparseJSON2 = aliftParseJSON2 aparseJSON aparseJSONList aparseJSON aparseJSONList
{-# INLINE aparseJSON2 #-}

-- -------------------------------------------------------------------------------
-- -- List functions
-- -------------------------------------------------------------------------------

-- | Helper function to use with 'liftParseJSON'. See 'Data.Aeson.ToJSON.listEncoding'.
alistParser :: Applicative m => (Value -> Parser (m a)) -> Value -> Parser (m [a])
alistParser f (Array xs) = fmap (sequenceA . V.toList) (V.mapM f xs)
alistParser _ v = typeMismatch "[a]" v
{-# INLINE alistParser #-}

-------------------------------------------------------------------------------
-- [] instances
-------------------------------------------------------------------------------

instance Applicative m => AFromJSON1 m [] where
    aliftParseJSON _ p' = p'
    {-# INLINE aliftParseJSON #-}

instance AFromJSON m a => AFromJSON m [a] where
    aparseJSON = aparseJSON1

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

-- | Retrieve the value associated with the given key of an 'Object'. The
-- result is 'Nothing' if the key is not present or if its value is 'Null',
-- or 'empty' if the value cannot be converted to the desired type.
--
-- This accessor is most useful if the key and value can be absent
-- from an object without affecting its validity.  If the key and
-- value are mandatory, use '.:' instead.
-- based on (.:?)
aparseFieldMaybe :: (Applicative m, AFromJSON m a) => Object -> Text -> Parser (m (Maybe a))
aparseFieldMaybe = aexplicitParseFieldMaybe aparseJSON
{-# INLINE aparseFieldMaybe #-}

-- | Variant of '.:?' with explicit parser function.
aexplicitParseFieldMaybe :: Applicative m => (Value -> Parser (m a)) -> Object -> Text -> Parser (m (Maybe a))
aexplicitParseFieldMaybe p obj key = case H.lookup key obj of
    Nothing -> pure (pure Nothing)
    Just v  -> aliftParseJSON p (alistParser p) v <?> Key key -- listParser isn't used by maybe instance.
{-# INLINE aexplicitParseFieldMaybe #-}

--------------------------------------------------------------------------------
-- Generic parseJSON
-------------------------------------------------------------------------------

instance Applicative m => AGFromJSON m arity V1 where
    -- Whereof we cannot format, thereof we cannot parse:
    agParseJSON _ _ _ = fail "Attempted to parse empty type"

instance {-# OVERLAPPABLE #-} (AGFromJSON m arity a) => AGFromJSON m arity (M1 i c a) where
    -- Meta-information, which is not handled elsewhere, is just added to the
    -- parsed value:
    agParseJSON opts fargs = fmap (fmap M1) . agParseJSON opts fargs

instance (AFromJSON m a) => AGFromJSON m arity (K1 i a) where
    -- Constant values are decoded using their FromJSON instance:
    agParseJSON _opts _ = fmap (fmap K1) . aparseJSON

instance Applicative m => AGFromJSON m One Par1 where
    -- Direct occurrences of the last type parameter are decoded with the
    -- function passed in as an argument:
    agParseJSON _opts (AFrom1Args pj _) = fmap (fmap Par1) . pj

instance (Applicative m, AFromJSON1 m f) => AGFromJSON m One (Rec1 f) where
    -- Recursive occurrences of the last type parameter are decoded using their
    -- FromJSON1 instance:
    agParseJSON _opts (AFrom1Args pj pjl) = fmap (fmap Rec1) . aliftParseJSON pj pjl

instance Applicative m => AGFromJSON m arity U1 where
    -- Empty constructors are expected to be encoded as an empty array:
    agParseJSON _opts _ v
        | _isEmptyArray v = pure (pure U1)
        | otherwise      = typeMismatch "unit constructor (U1)" v
      where
        -- | Determines if the 'Value' is an empty 'Array'.
        -- Note that: @isEmptyArray 'emptyArray'@.
        _isEmptyArray :: Value -> Bool
        _isEmptyArray (Array arr) = V.null arr
        _isEmptyArray _ = False

instance ( Applicative m, AConsFromJSON m arity a
         , AllNullary (C1 c a) allNullary
         , AParseSum m arity (C1 c a) allNullary
         ) => AGFromJSON m arity (D1 d (C1 c a)) where
    -- The option 'tagSingleConstructors' determines whether to wrap
    -- a single-constructor type.
    agParseJSON opts fargs
#if MIN_VERSION_aeson(1,3,0)
        | tagSingleConstructors opts
            = fmap (fmap M1)
            . (unTagged :: Tagged allNullary (Parser (m (C1 c a p))) -> Parser (m (C1 c a p)))
            . aparseSum opts fargs
#endif
        | otherwise = fmap (fmap (M1 . M1)) . aconsParseJSON opts fargs

instance (Applicative m, AConsFromJSON m arity a) => AGFromJSON m arity (C1 c a) where
    -- Constructors need to be decoded differently depending on whether they're
    -- a record or not. This distinction is made by consParseJSON:
    agParseJSON opts fargs = fmap (fmap M1) . aconsParseJSON opts fargs

instance ( Applicative m, AFromProduct m arity a, AFromProduct m arity b
         , ProductSize a, ProductSize b
         ) => AGFromJSON m arity (a :*: b) where
    -- Products are expected to be encoded to an array. Here we check whether we
    -- got an array of the same size as the product, then parse each of the
    -- product's elements using parseProduct:
    agParseJSON opts fargs = withArray "product (:*:)" $ \arr ->
      let lenArray = V.length arr
          lenProduct = (unTagged2 :: Tagged2 (a :*: b) Int -> Int)
                       productSize in
      if lenArray == lenProduct
      then aparseProduct opts fargs arr 0 lenProduct
      else fail $ "When expecting a product of " ++ show lenProduct ++
                  " values, encountered an Array of " ++ show lenArray ++
                  " elements instead"

instance ( Applicative m
         , AllNullary (a :+: b) allNullary
         , AParseSum m arity (a :+: b) allNullary
         ) => AGFromJSON m arity (a :+: b) where
    -- If all constructors of a sum datatype are nullary and the
    -- 'allNullaryToStringTag' option is set they are expected to be
    -- encoded as strings.  This distinction is made by 'parseSum':
    agParseJSON opts fargs =
      (unTagged :: Tagged allNullary (Parser (m ((a :+: b) d))) ->
                                     Parser (m ((a :+: b) d)))
                 . aparseSum opts fargs

instance (AFromJSON1 m f, AGFromJSON m One g) => AGFromJSON m One (f :.: g) where
    -- If an occurrence of the last type parameter is nested inside two
    -- composed types, it is decoded by using the outermost type's FromJSON1
    -- instance to generically decode the innermost type:
    agParseJSON opts fargs =
      let gpj = agParseJSON opts fargs in
      fmap (fmap Comp1) . aliftParseJSON gpj (alistParser gpj)

--------------------------------------------------------------------------------

class AParseSum m arity f allNullary where
    aparseSum :: Options -> AFromArgs m arity a
             -> Value -> Tagged allNullary (Parser (m (f a)))

instance ( ASumFromString m f
         , AFromPair m arity f
         , AFromTaggedObject m arity f
         , AFromUntaggedValue m arity f
         ) => AParseSum m arity f 'True where
    aparseSum opts fargs
        | allNullaryToStringTag opts = Tagged . aparseAllNullarySum opts
        | otherwise                  = Tagged . aparseNonAllNullarySum opts fargs

instance ( AFromPair m arity f
         , AFromTaggedObject m arity f
         , AFromUntaggedValue m arity f
         ) => AParseSum m arity f 'False where
    aparseSum opts fargs = Tagged . aparseNonAllNullarySum opts fargs

--------------------------------------------------------------------------------

aparseAllNullarySum :: ASumFromString m f => Options -> Value -> Parser (m (f a))
aparseAllNullarySum opts = withText "Text" $ \key ->
                            maybe (notFound key) pure $
                              aparseSumFromString opts key

class ASumFromString m f where
    aparseSumFromString :: Options -> Text -> Maybe (m (f a))

instance (Functor m, ASumFromString m a, ASumFromString m b) => ASumFromString m (a :+: b) where
    aparseSumFromString opts key = (fmap L1 <$> aparseSumFromString opts key) <|>
                                  (fmap R1 <$> aparseSumFromString opts key)

instance (Applicative m, Constructor c) => ASumFromString m (C1 c U1) where
    aparseSumFromString opts key | key == name = Just $ pure $ M1 U1
                                 | otherwise   = Nothing
        where
          name = pack $ constructorTagModifier opts $
                          conName (undefined :: t c U1 p)

--------------------------------------------------------------------------------

aparseNonAllNullarySum :: ( AFromPair m arity f
                         , AFromTaggedObject m arity f
                         , AFromUntaggedValue m arity f
                         ) => Options -> AFromArgs m arity c
                           -> Value -> Parser (m (f c))
aparseNonAllNullarySum opts fargs =
    case sumEncoding opts of
      TaggedObject{..} ->
          withObject "Object" $ \obj -> do
            tag <- obj .: pack tagFieldName
            fromMaybe (notFound tag) $
              aparseFromTaggedObject opts fargs contentsFieldName obj tag

      ObjectWithSingleField ->
          withObject "Object" $ \obj ->
            case H.toList obj of
              [pr@(tag, _)] -> fromMaybe (notFound tag) $
                                   aparsePair opts fargs pr
              _ -> fail "Object doesn't have a single field"

      TwoElemArray ->
          withArray "Array" $ \arr ->
            if V.length arr == 2
            then case V.unsafeIndex arr 0 of
                   String tag -> fromMaybe (notFound tag) $
                                   aparsePair opts fargs (tag, V.unsafeIndex arr 1)
                   _ -> fail "First element is not a String"
            else fail "Array doesn't have 2 elements"

      UntaggedValue -> aparseUntaggedValue opts fargs

--------------------------------------------------------------------------------

class AFromTaggedObject m arity f where
    aparseFromTaggedObject :: Options -> AFromArgs m arity a
                          -> String -> Object
                          -> Text -> Maybe (Parser (m (f a)))

instance ( Functor m, AFromTaggedObject m arity a, AFromTaggedObject m arity b) =>
    AFromTaggedObject m arity (a :+: b) where
        aparseFromTaggedObject opts fargs contentsFieldName obj tag =
            (fmap (fmap L1) <$> aparseFromTaggedObject opts fargs contentsFieldName obj tag) <|>
            (fmap (fmap R1) <$> aparseFromTaggedObject opts fargs contentsFieldName obj tag)

instance ( Functor m, AFromTaggedObject' m arity f
         , Constructor c
         ) => AFromTaggedObject m arity (C1 c f) where
    aparseFromTaggedObject opts fargs contentsFieldName obj tag
        | tag == name = Just $ (fmap M1) <$> aparseFromTaggedObject'
                                        opts fargs contentsFieldName obj
        | otherwise = Nothing
        where
          name = pack $ constructorTagModifier opts $
                          conName (undefined :: t c f p)

--------------------------------------------------------------------------------

class AFromTaggedObject' m arity f where
    aparseFromTaggedObject' :: Options -> AFromArgs m arity a -> String
                           -> Object -> Parser (m (f a))

class AFromTaggedObject'' m arity f isRecord where
    aparseFromTaggedObject'' :: Options -> AFromArgs m arity a -> String
                            -> Object -> Tagged isRecord (Parser (m (f a)))

instance ( IsRecord f isRecord
         , AFromTaggedObject'' m arity f isRecord
         ) => AFromTaggedObject' m arity f where
    aparseFromTaggedObject' opts fargs contentsFieldName =
        (unTagged :: Tagged isRecord (Parser (m (f a))) -> Parser (m (f a))) .
        aparseFromTaggedObject'' opts fargs contentsFieldName

instance (AFromRecord m arity f) => AFromTaggedObject'' m arity f 'True where
    aparseFromTaggedObject'' opts fargs _ =
      Tagged . aparseRecord opts fargs

instance (AGFromJSON m arity f) => AFromTaggedObject'' m arity f 'False where
    aparseFromTaggedObject'' opts fargs contentsFieldName = Tagged .
      (agParseJSON opts fargs <=< (.: pack contentsFieldName))

instance {-# OVERLAPPING #-} Applicative m => AFromTaggedObject'' m arity U1 'False where
    aparseFromTaggedObject'' _ _ _ _ = Tagged (pure (pure U1))

--------------------------------------------------------------------------------

class AConsFromJSON m arity f where
    aconsParseJSON  :: Options -> AFromArgs m arity a
                   -> Value -> Parser (m (f a))

class AConsFromJSON' m arity f isRecord where
    aconsParseJSON' :: Options -> AFromArgs m arity a
                   -> Value -> Tagged isRecord (Parser (m (f a)))

instance ( IsRecord f isRecord
         , AConsFromJSON' m arity f isRecord
         ) => AConsFromJSON m arity f where
    aconsParseJSON opts fargs =
      (unTagged :: Tagged isRecord (Parser (m (f a))) -> Parser (m (f a)))
        . aconsParseJSON' opts fargs

instance {-# OVERLAPPING #-}
         ( AGFromJSON m arity a, AFromRecord m arity (S1 s a)
         ) => AConsFromJSON' m arity (S1 s a) 'True where
    aconsParseJSON' opts fargs
      | unwrapUnaryRecords opts = Tagged . agParseJSON opts fargs
      | otherwise = Tagged . withObject "unary record" (aparseRecord opts fargs)

instance AFromRecord m arity f => AConsFromJSON' m arity f 'True where
    aconsParseJSON' opts fargs =
      Tagged . withObject "record (:*:)" (aparseRecord opts fargs)

instance AGFromJSON m arity f => AConsFromJSON' m arity f 'False where
    aconsParseJSON' opts fargs = Tagged . agParseJSON opts fargs

--------------------------------------------------------------------------------

class AFromRecord m arity f where
    aparseRecord :: Options -> AFromArgs m arity a
                -> Object -> Parser (m (f a))

instance
    ( Applicative m
    , AFromRecord m arity a
    , AFromRecord m arity b
    ) => AFromRecord m arity (a :*: b) where
    aparseRecord opts fargs obj = getCompose $
      (:*:) <$> Compose (aparseRecord opts fargs obj)
            <*> Compose (aparseRecord opts fargs obj)

instance {-# OVERLAPPABLE #-} (Selector s, AGFromJSON m arity a) =>
  AFromRecord m arity (S1 s a) where
    aparseRecord opts fargs =
      (<?> Key label) . agParseJSON opts fargs <=< (.: label)
        where
          label = pack . fieldLabelModifier opts $ selName (undefined :: t s a p)

instance {-# INCOHERENT #-} (Selector s, AFromJSON m a) =>
  AFromRecord m arity (S1 s (K1 i (Maybe a))) where
    aparseRecord opts _ obj = fmap (M1 . K1) <$> obj `aparseFieldMaybe` pack label
        where
          label = fieldLabelModifier opts $
                    selName (undefined :: t s (K1 i (Maybe a)) p)

-- Parse an Option like a Maybe.
instance {-# INCOHERENT #-} (Selector s, AFromJSON m a) =>
  AFromRecord m arity (S1 s (K1 i (Semigroup.Option a))) where
    aparseRecord opts fargs obj = fmap wrap <$> aparseRecord opts fargs obj
      where
        wrap :: S1 s (K1 i (Maybe a)) p -> S1 s (K1 i (Semigroup.Option a)) p
        wrap (M1 (K1 a)) = M1 (K1 (Semigroup.Option a))

--------------------------------------------------------------------------------

class AFromProduct m arity f where
    aparseProduct :: Options -> AFromArgs m arity a
                 -> Array -> Int -> Int
                 -> Parser (m (f a))

instance ( Applicative m
         , AFromProduct m arity a
         , AFromProduct m arity b
         ) => AFromProduct m arity (a :*: b) where
    aparseProduct opts fargs arr ix len = getCompose $
        (:*:) <$> Compose (aparseProduct opts fargs arr ix  lenL)
              <*> Compose (aparseProduct opts fargs arr ixR lenR)
        where
          lenL = len `unsafeShiftR` 1
          ixR  = ix + lenL
          lenR = len - lenL

instance (AGFromJSON m arity a) => AFromProduct m arity (S1 s a) where
    aparseProduct opts fargs arr ix _ =
      agParseJSON opts fargs $ V.unsafeIndex arr ix

--------------------------------------------------------------------------------

class AFromPair m arity f where
    aparsePair :: Options -> AFromArgs m arity a
              -> Pair -> Maybe (Parser (m (f a)))

instance
    ( Functor m
    , AFromPair m arity a
    , AFromPair m arity b
    ) => AFromPair m arity (a :+: b) where
    aparsePair opts fargs pr = (fmap (fmap L1) <$> aparsePair opts fargs pr) <|>
                                (fmap (fmap R1) <$> aparsePair opts fargs pr)

instance ( Constructor c
         , AGFromJSON m arity a
         , AConsFromJSON m arity a
         ) => AFromPair m arity (C1 c a) where
    aparsePair opts fargs (tag, value)
        | tag == tag' = Just $ agParseJSON opts fargs value
        | otherwise   = Nothing
        where
          tag' = pack $ constructorTagModifier opts $
                          conName (undefined :: t c a p)

--------------------------------------------------------------------------------

class AFromUntaggedValue m arity f where
    aparseUntaggedValue :: Options -> AFromArgs m arity a
                       -> Value -> Parser (m (f a))

instance
    ( Functor m
    , AFromUntaggedValue m arity a
    , AFromUntaggedValue m arity b
    ) => AFromUntaggedValue m arity (a :+: b)
  where
    aparseUntaggedValue opts fargs value =
        fmap L1 <$> aparseUntaggedValue opts fargs value <|>
        fmap R1 <$> aparseUntaggedValue opts fargs value

instance {-# OVERLAPPABLE #-}
    ( AGFromJSON m arity a
    , AConsFromJSON m arity a
    ) => AFromUntaggedValue m arity (C1 c a)
  where
    aparseUntaggedValue = agParseJSON

instance {-# OVERLAPPING #-}
    ( Applicative m, Constructor c )
    => AFromUntaggedValue m arity (C1 c U1)
  where
    aparseUntaggedValue opts _ (String s)
        | s == pack (constructorTagModifier opts (conName (undefined :: t c U1 p))) =
            pure $ pure $ M1 U1
        | otherwise =
            fail $ "Invalid tag: " ++ unpack s
    aparseUntaggedValue _ _ v = typeMismatch (conName (undefined :: t c U1 p)) v

--------------------------------------------------------------------------------

notFound :: Text -> Parser a
notFound key = fail $ "The key \"" ++ unpack key ++ "\" was not found"
{-# INLINE notFound #-}


-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- base
-------------------------------------------------------------------------------

instance Applicative m => AFromJSON2 m Const where
    aliftParseJSON2 p _ _ _ = fmap (fmap Const) . p
    {-# INLINE aliftParseJSON2 #-}

instance AFromJSON m a => AFromJSON1 m (Const a) where
    aliftParseJSON _ _ = fmap (fmap Const) . aparseJSON
    {-# INLINE aliftParseJSON #-}

instance AFromJSON m a => AFromJSON m (Const a b) where
    {-# INLINE aparseJSON #-}
    aparseJSON = fmap (fmap Const) . aparseJSON


instance Applicative m => AFromJSON1 m Maybe where
    aliftParseJSON _ _ Null = pure (pure Nothing)
    aliftParseJSON p _ a    = fmap Just <$> p a
    {-# INLINE aliftParseJSON #-}

instance (AFromJSON m a) => AFromJSON m (Maybe a) where
    aparseJSON = aparseJSON1
    {-# INLINE aparseJSON #-}


instance Applicative m => AFromJSON2 m Either where
    aliftParseJSON2 pA _ pB _ (Object (H.toList -> [(key, value)]))
        | key == left  = (fmap Left)  <$> pA value <?> Key left
        | key == right = (fmap Right) <$> pB value <?> Key right
      where
        left, right :: Text
        left  = "Left"
        right = "Right"

    aliftParseJSON2 _ _ _ _ _ = fail $
        "expected an object with a single property " ++
        "where the property key should be either " ++
        "\"Left\" or \"Right\""
    {-# INLINE aliftParseJSON2 #-}

instance (AFromJSON m a) => AFromJSON1 m (Either a) where
    aliftParseJSON = aliftParseJSON2 aparseJSON aparseJSONList
    {-# INLINE aliftParseJSON #-}

instance (AFromJSON m a, AFromJSON m b) => AFromJSON m (Either a b) where
    aparseJSON = aparseJSON2
    {-# INLINE aparseJSON #-}


instance Applicative m => AFromJSON m Void where
    aparseJSON _ = fail "Cannot parse Void"
    {-# INLINE aparseJSON #-}

instance Applicative m => AFromJSON m Bool where
    aparseJSON = fmap pure . parseJSON
    {-# INLINE aparseJSON #-}

instance Applicative m => AFromJSON m Ordering where
    aparseJSON = fmap pure . parseJSON

instance Applicative m => AFromJSON m () where
    aparseJSON = fmap pure . parseJSON
    {-# INLINE aparseJSON #-}

instance Applicative m => AFromJSON m Char where
    aparseJSON = fmap pure . parseJSON
    {-# INLINE aparseJSON #-}

    aparseJSONList = fmap pure . parseJSONList
    {-# INLINE aparseJSONList #-}

instance Applicative m => AFromJSON m Double where
    aparseJSON = fmap pure . parseJSON
    {-# INLINE aparseJSON #-}


instance Applicative m => AFromJSON m Float where
    aparseJSON = fmap pure . parseJSON
    {-# INLINE aparseJSON #-}

instance (Applicative m, FromJSON a, Integral a) => AFromJSON m (Ratio a) where
    aparseJSON = fmap pure . parseJSON

-- | This instance includes a bounds check to prevent maliciously
-- large inputs to fill up the memory of the target system. You can
-- newtype 'Scientific' and provide your own instance using
-- 'withScientific' if you want to allow larger inputs.
instance (Applicative m, HasResolution a) => AFromJSON m (Fixed a) where
    aparseJSON = fmap pure . parseJSON
    {-# INLINE aparseJSON #-}

instance Applicative m => AFromJSON m Int where
    aparseJSON = fmap pure . parseJSON
    {-# INLINE aparseJSON #-}

-- | This instance includes a bounds check to prevent maliciously
-- large inputs to fill up the memory of the target system. You can
-- newtype 'Scientific' and provide your own instance using
-- 'withScientific' if you want to allow larger inputs.
instance Applicative m => AFromJSON m Integer where
    aparseJSON = fmap pure . parseJSON
    {-# INLINE aparseJSON #-}


instance Applicative m => AFromJSON m Natural where
    aparseJSON = fmap pure . parseJSON


instance Applicative m => AFromJSON m Int8 where
    aparseJSON = fmap pure . parseJSON
    {-# INLINE aparseJSON #-}


instance Applicative m => AFromJSON m Int16 where
    aparseJSON = fmap pure . parseJSON
    {-# INLINE aparseJSON #-}


instance Applicative m => AFromJSON m Int32 where
    aparseJSON = fmap pure . parseJSON
    {-# INLINE aparseJSON #-}


instance Applicative m => AFromJSON m Int64 where
    aparseJSON = fmap pure . parseJSON
    {-# INLINE aparseJSON #-}


instance Applicative m => AFromJSON m Word where
    aparseJSON = fmap pure . parseJSON
    {-# INLINE aparseJSON #-}


instance Applicative m => AFromJSON m Word8 where
    aparseJSON = fmap pure . parseJSON
    {-# INLINE aparseJSON #-}


instance Applicative m => AFromJSON m Word16 where
    aparseJSON = fmap pure . parseJSON
    {-# INLINE aparseJSON #-}


instance Applicative m => AFromJSON m Word32 where
    aparseJSON = fmap pure . parseJSON
    {-# INLINE aparseJSON #-}


instance Applicative m => AFromJSON m Word64 where
    aparseJSON = fmap pure . parseJSON
    {-# INLINE aparseJSON #-}


instance Applicative m => AFromJSON m CTime where
    aparseJSON = fmap (fmap CTime) . aparseJSON
    {-# INLINE aparseJSON #-}


instance Applicative m => AFromJSON m Text where
    aparseJSON = fmap pure . parseJSON
    {-# INLINE aparseJSON #-}


instance Applicative m => AFromJSON m LT.Text where
    aparseJSON = fmap pure . parseJSON
    {-# INLINE aparseJSON #-}


instance Applicative m => AFromJSON m Version where
    aparseJSON = fmap pure . parseJSON
    {-# INLINE aparseJSON #-}


-------------------------------------------------------------------------------
-- semigroups NonEmpty
-------------------------------------------------------------------------------

instance Applicative m => AFromJSON1 m NonEmpty where
    aliftParseJSON p _ = withArray "NonEmpty a" $
        \arr -> do
            let as = V.toList arr
            case as of
                [] -> fail "Expected a NonEmpty but got an empty list"
                (x : xs) -> getCompose $
                    (:|) <$> (Compose $ _parseIndexedJSON p 0 x)
                         <*> (sequenceA $ Compose <$> zipWith (_parseIndexedJSON p) [1..] xs)
    {-# INLINE aliftParseJSON #-}

instance (AFromJSON m a) => AFromJSON m (NonEmpty a) where
    aparseJSON = aparseJSON1
    {-# INLINE aparseJSON #-}

-------------------------------------------------------------------------------
-- scientific
-------------------------------------------------------------------------------

instance Applicative m => AFromJSON m Scientific where
    aparseJSON = fmap pure . parseJSON
    {-# INLINE aparseJSON #-}

-------------------------------------------------------------------------------
-- DList
-------------------------------------------------------------------------------

instance Applicative m => AFromJSON1 m DList.DList where
    aliftParseJSON p _ = withArray "DList a" $
      getCompose . fmap DList.fromList .
      sequenceA . fmap Compose . zipWith (_parseIndexedJSON p) [0..] . V.toList
    {-# INLINE aliftParseJSON #-}

instance (AFromJSON m a) => AFromJSON m (DList.DList a) where
    aparseJSON = aparseJSON1
    {-# INLINE aparseJSON #-}

-------------------------------------------------------------------------------
-- tranformers - Functors
-------------------------------------------------------------------------------

instance Applicative m => AFromJSON1 m Identity where
    aliftParseJSON p _ a = fmap Identity <$> p a
    {-# INLINE aliftParseJSON #-}

    aliftParseJSONList _ p a = fmap (fmap Identity) <$> p a
    {-# INLINE aliftParseJSONList #-}

instance (AFromJSON m a) => AFromJSON m (Identity a) where
    aparseJSON = aparseJSON1
    {-# INLINE aparseJSON #-}

    aparseJSONList = aliftParseJSONList aparseJSON aparseJSONList
    {-# INLINE aparseJSONList #-}


instance (AFromJSON1 m  f, AFromJSON1 m g) => AFromJSON1 m (Compose f g) where
    aliftParseJSON p pl a = fmap Compose <$> aliftParseJSON g gl a
      where
        g  = aliftParseJSON p pl
        gl = aliftParseJSONList p pl
    {-# INLINE aliftParseJSON #-}

    aliftParseJSONList p pl a = fmap (fmap Compose) <$> aliftParseJSONList g gl a
      where
        g  = aliftParseJSON p pl
        gl = aliftParseJSONList p pl
    {-# INLINE aliftParseJSONList #-}

instance (AFromJSON1 m  f, AFromJSON1 m g, AFromJSON m a) => AFromJSON m (Compose f g a) where
    aparseJSON = aparseJSON1
    {-# INLINE aparseJSON #-}

    aparseJSONList = aliftParseJSONList aparseJSON aparseJSONList
    {-# INLINE aparseJSONList #-}


instance (AFromJSON1 m  f, AFromJSON1 m g) => AFromJSON1 m (Product f g) where
    aliftParseJSON p pl a = fmap (uncurry Pair) <$> aliftParseJSON2 px pxl py pyl a
      where
        px  = aliftParseJSON p pl
        pxl = aliftParseJSONList p pl
        py  = aliftParseJSON p pl
        pyl = aliftParseJSONList p pl
    {-# INLINE aliftParseJSON #-}

instance (AFromJSON1 m f, AFromJSON1 m g, AFromJSON m a) => AFromJSON m (Product f g a) where
    aparseJSON = aparseJSON1
    {-# INLINE aparseJSON #-}


instance (AFromJSON1 m  f, AFromJSON1 m g) => AFromJSON1 m (Sum f g) where
    aliftParseJSON p pl (Object (H.toList -> [(key, value)]))
        | key == inl = fmap InL <$> aliftParseJSON p pl value <?> Key inl
        | key == inr = fmap InR <$> aliftParseJSON p pl value <?> Key inl
      where
        inl, inr :: Text
        inl = "InL"
        inr = "InR"

    aliftParseJSON _ _ _ = fail $
        "expected an object with a single property " ++
        "where the property key should be either " ++
        "\"InL\" or \"InR\""
    {-# INLINE aliftParseJSON #-}

instance (AFromJSON1 m  f, AFromJSON1 m g, AFromJSON m a) => AFromJSON m (Sum f g a) where
    aparseJSON = aparseJSON1
    {-# INLINE aparseJSON #-}

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------

instance Applicative m => AFromJSON1 m Seq.Seq where
    aliftParseJSON p _ = withArray "Seq a" $
      getCompose . fmap Seq.fromList .
      sequenceA . fmap Compose . zipWith (_parseIndexedJSON p) [0..] . V.toList
    {-# INLINE aliftParseJSON #-}

instance (AFromJSON m a) => AFromJSON m (Seq.Seq a) where
    aparseJSON = aparseJSON1
    {-# INLINE aparseJSON #-}


instance (Ord a, AFromJSON m a) => AFromJSON m (Set.Set a) where
    aparseJSON = fmap (fmap Set.fromList) . aparseJSON
    {-# INLINE aparseJSON #-}


instance Applicative m => AFromJSON m IntSet.IntSet where
    aparseJSON = fmap (fmap IntSet.fromList) . aparseJSON
    {-# INLINE aparseJSON #-}


instance Applicative m => AFromJSON1 m IntMap.IntMap where
    aliftParseJSON p pl = fmap (fmap IntMap.fromList) . aliftParseJSON p' pl'
      where
        p'  = aliftParseJSON2     aparseJSON aparseJSONList p pl
        pl' = aliftParseJSONList2 aparseJSON aparseJSONList p pl
    {-# INLINE aliftParseJSON #-}

instance AFromJSON m a => AFromJSON m (IntMap.IntMap a) where
    aparseJSON = fmap (fmap IntMap.fromList) . aparseJSON
    {-# INLINE aparseJSON #-}


instance (Applicative m, FromJSONKey k, Ord k) => AFromJSON1 m (M.Map k) where
    aliftParseJSON p _ = case fromJSONKey of
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
    {-# INLINE aliftParseJSON #-}

instance (FromJSONKey k, Ord k, AFromJSON m v) => AFromJSON m (M.Map k v) where
    aparseJSON = aparseJSON1
    {-# INLINE aparseJSON #-}


instance Applicative m => AFromJSON1 m Tree.Tree where
    aliftParseJSON p pl = go
      where
        go v = fmap (uncurry Tree.Node) <$> aliftParseJSON2 p pl p' pl' v

        p' = aliftParseJSON go (alistParser go)
        pl'= aliftParseJSONList go (alistParser go)

instance (AFromJSON m v) => AFromJSON m (Tree.Tree v) where
    aparseJSON = aparseJSON1
    {-# INLINE aparseJSON #-}

-------------------------------------------------------------------------------
-- uuid
-------------------------------------------------------------------------------

instance Applicative m => AFromJSON m UUID.UUID where
    aparseJSON = fmap pure . parseJSON

-------------------------------------------------------------------------------
-- vector
-------------------------------------------------------------------------------

instance Applicative m => AFromJSON1 m Vector where
    aliftParseJSON p _ = withArray "Vector a" $ getCompose .
        traverse (Compose . uncurry (_parseIndexedJSON p)) . V.indexed
    {-# INLINE aliftParseJSON #-}

instance (AFromJSON m a) => AFromJSON m (Vector a) where
    aparseJSON = aparseJSON1
    {-# INLINE aparseJSON #-}


mvectorParseJSON :: (AFromJSON m a, VG.Vector w a) => String -> Value -> Parser (m (w a))
mvectorParseJSON s = withArray s $ getCompose . fmap V.convert
    . traverse (Compose . uncurry (_parseIndexedJSON aparseJSON)) . V.indexed
{-# INLINE mvectorParseJSON #-}

instance (Storable a, AFromJSON m a) => AFromJSON m (VS.Vector a) where
    aparseJSON = mvectorParseJSON "Data.Vector.Storable.Vector a"

instance (VP.Prim a, AFromJSON m a) => AFromJSON m (VP.Vector a) where
    aparseJSON = mvectorParseJSON "Data.Vector.Primitive.Vector a"
    {-# INLINE aparseJSON #-}

instance (VG.Vector VU.Vector a, AFromJSON m a) => AFromJSON m (VU.Vector a) where
    aparseJSON = mvectorParseJSON "Data.Vector.Unboxed.Vector a"
    {-# INLINE aparseJSON #-}

-------------------------------------------------------------------------------
-- unordered-containers
-------------------------------------------------------------------------------

instance (Eq a, Hashable a, AFromJSON m a) => AFromJSON m (HashSet.HashSet a) where
    aparseJSON = fmap (fmap HashSet.fromList) . aparseJSON
    {-# INLINE aparseJSON #-}


instance (Applicative m, FromJSONKey k, Eq k, Hashable k) => AFromJSON1 m (H.HashMap k) where
    aliftParseJSON p _ = case fromJSONKey of
        FromJSONKeyCoerce _ -> withObject "HashMap ~Text v" $
            H.foldrWithKey (\k v m -> getCompose $ H.insert
                <$> (Compose $ (pure . pure $ unsafeCoerce k) <?> Key k)
                <*> (Compose $ p v <?> Key k)
                <*> Compose m)
                (pure $ pure H.empty)

        FromJSONKeyText f -> withObject "HashMap k v" $
            H.foldrWithKey (\k v m -> getCompose $ H.insert
                <$> (Compose $ (pure . pure $ f k) <?> Key k)
                <*> (Compose $ p v <?> Key k)
                <*> Compose m)
                (pure $ pure H.empty)

        FromJSONKeyTextParser f -> withObject "HashMap k v" $
            H.foldrWithKey (\k v m -> getCompose $ H.insert
                <$> (Compose $ (pure <$> f k) <?> Key k)
                <*> (Compose $ p v <?> Key k)
                <*> Compose m)
                (pure $ pure H.empty)

        FromJSONKeyValue f -> withArray "HashMap k v" $ \arr ->
            getCompose $ H.fromList <$> (
                -- [Compose (a, b)] -> Compose [(a, b)]
                sequenceA
                . fmap Compose
                -- [Parser m (a, b)]
                . zipWith (_parseIndexedJSONPair ((<$>) . (,)) f p) [0..] . V.toList $ arr)

instance (AFromJSON m  v, FromJSONKey k, Eq k, Hashable k) => AFromJSON m (H.HashMap k v) where
    aparseJSON = aparseJSON1
    {-# INLINE aparseJSON #-}

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

instance Applicative m => AFromJSON m Value where
    aparseJSON = pure . pure
    {-# INLINE aparseJSON #-}

instance Applicative m => AFromJSON m DotNetTime where
    aparseJSON = fmap pure . parseJSON

-------------------------------------------------------------------------------
-- primitive
-------------------------------------------------------------------------------

#if MIN_VERSION_primitive(0,6,4)
instance AFromJSON m a => AFromJSON m (PM.Array a) where
  -- note: we could do better than this if vector exposed the data
  -- constructor in Data.Vector.
  aparseJSON = fmap (fmap Exts.fromList) . aparseJSON

instance AFromJSON m a => AFromJSON m (PM.SmallArray a) where
  aparseJSON = fmap (fmap Exts.fromList) . aparseJSON

instance (PM.Prim a,FromJSON a) => AFromJSON m (PM.PrimArray a) where
  aparseJSON = fmap (fmap Exts.fromList) . aparseJSON

instance (PM.PrimUnlifted a,FromJSON a) => AFromJSON m (PM.UnliftedArray a) where
  aparseJSON = fmap (fmap Exts.fromList) . aparseJSON
#endif

-------------------------------------------------------------------------------
-- time
-------------------------------------------------------------------------------

instance Applicative m => AFromJSON m Day where
    aparseJSON = fmap pure . parseJSON


instance Applicative m => AFromJSON m TimeOfDay where
    aparseJSON = fmap pure . parseJSON


instance Applicative m => AFromJSON m LocalTime where
    aparseJSON = fmap pure . parseJSON


-- | Supported string formats:
--
-- @YYYY-MM-DD HH:MM Z@
-- @YYYY-MM-DD HH:MM:SS Z@
-- @YYYY-MM-DD HH:MM:SS.SSS Z@
--
-- The first space may instead be a @T@, and the second space is
-- optional.  The @Z@ represents UTC.  The @Z@ may be replaced with a
-- time zone offset of the form @+0000@ or @-08:00@, where the first
-- two digits are hours, the @:@ is optional and the second two digits
-- (also optional) are minutes.
instance Applicative m => AFromJSON m ZonedTime where
    aparseJSON = fmap pure . parseJSON


instance Applicative m => AFromJSON m UTCTime where
    aparseJSON = fmap pure . parseJSON


-- | This instance includes a bounds check to prevent maliciously
-- large inputs to fill up the memory of the target system. You can
-- newtype 'Scientific' and provide your own instance using
-- 'withScientific' if you want to allow larger inputs.
instance Applicative m => AFromJSON m NominalDiffTime where
    aparseJSON = fmap pure . parseJSON
    {-# INLINE aparseJSON #-}


-- | This instance includes a bounds check to prevent maliciously
-- large inputs to fill up the memory of the target system. You can
-- newtype 'Scientific' and provide your own instance using
-- 'withScientific' if you want to allow larger inputs.
instance Applicative m => AFromJSON m DiffTime where
    aparseJSON = _withBoundedScientific "NominalDiffTime" $ pure . pure . realToFrac
      where
        -- | @'withBoundedScientific' expected f value@ applies @f@ to the 'Scientific' number
        -- when @value@ is a 'Number' and fails using @'typeMismatch' expected@
        -- otherwise.
        --
        -- The conversion will also fail with a @'typeMismatch' if the
        -- 'Scientific' exponent is larger than 1024.
        _withBoundedScientific :: String -> (Scientific -> Parser a) -> Value -> Parser a
        _withBoundedScientific _ f v@(Number scientific) =
            if base10Exponent scientific > 1024
            then typeMismatch "a number with exponent <= 1024" v
            else f scientific
        _withBoundedScientific expected _ v                   = typeMismatch expected v
        {-# INLINE _withBoundedScientific #-}
    {-# INLINE aparseJSON #-}
-------------------------------------------------------------------------------
-- base Monoid/Semigroup
-------------------------------------------------------------------------------

instance Applicative m => AFromJSON1 m Monoid.Dual where
    aliftParseJSON p _ = fmap (fmap Monoid.Dual) . p
    {-# INLINE aliftParseJSON #-}

instance AFromJSON m a => AFromJSON m (Monoid.Dual a) where
    aparseJSON = aparseJSON1
    {-# INLINE aparseJSON #-}


instance Applicative m => AFromJSON1 m Monoid.First where
    aliftParseJSON p p' = fmap (fmap Monoid.First) . aliftParseJSON p p'
    {-# INLINE aliftParseJSON #-}

instance AFromJSON m a => AFromJSON m (Monoid.First a) where
    aparseJSON = aparseJSON1
    {-# INLINE aparseJSON #-}


instance Applicative m => AFromJSON1 m Monoid.Last where
    aliftParseJSON p p' = fmap (fmap Monoid.Last) . aliftParseJSON p p'
    {-# INLINE aliftParseJSON #-}

instance AFromJSON m a => AFromJSON m (Monoid.Last a) where
    aparseJSON = aparseJSON1
    {-# INLINE aparseJSON #-}


instance Applicative m => AFromJSON1 m Semigroup.Min where
    aliftParseJSON p _ a = fmap Semigroup.Min <$> p a
    {-# INLINE aliftParseJSON #-}

    aliftParseJSONList _ p a = fmap (fmap Semigroup.Min) <$> p a
    {-# INLINE aliftParseJSONList #-}

instance (AFromJSON m a) => AFromJSON m (Semigroup.Min a) where
    aparseJSON = aparseJSON1
    {-# INLINE aparseJSON #-}

    aparseJSONList = aliftParseJSONList aparseJSON aparseJSONList
    {-# INLINE aparseJSONList #-}


instance Applicative m => AFromJSON1 m Semigroup.Max where
    aliftParseJSON p _ a = fmap Semigroup.Max <$> p a
    {-# INLINE aliftParseJSON #-}

    aliftParseJSONList _ p a = fmap (fmap Semigroup.Max) <$> p a
    {-# INLINE aliftParseJSONList #-}

instance (AFromJSON m a) => AFromJSON m (Semigroup.Max a) where
    aparseJSON = aparseJSON1
    {-# INLINE aparseJSON #-}

    aparseJSONList = aliftParseJSONList aparseJSON aparseJSONList
    {-# INLINE aparseJSONList #-}


instance Applicative m => AFromJSON1 m Semigroup.First where
    aliftParseJSON p _ a = fmap Semigroup.First <$> p a
    {-# INLINE aliftParseJSON #-}

    aliftParseJSONList _ p a = fmap (fmap Semigroup.First) <$> p a
    {-# INLINE aliftParseJSONList #-}

instance (AFromJSON m a) => AFromJSON m (Semigroup.First a) where
    aparseJSON = aparseJSON1
    {-# INLINE aparseJSON #-}

    aparseJSONList = aliftParseJSONList aparseJSON aparseJSONList
    {-# INLINE aparseJSONList #-}


instance Applicative m => AFromJSON1 m Semigroup.Last where
    aliftParseJSON p _ a = fmap Semigroup.Last <$> p a
    {-# INLINE aliftParseJSON #-}

    aliftParseJSONList _ p a = fmap (fmap Semigroup.Last) <$> p a
    {-# INLINE aliftParseJSONList #-}

instance (AFromJSON m a) => AFromJSON m (Semigroup.Last a) where
    aparseJSON = aparseJSON1
    {-# INLINE aparseJSON #-}

    aparseJSONList = aliftParseJSONList aparseJSON aparseJSONList
    {-# INLINE aparseJSONList #-}


instance Applicative m => AFromJSON1 m Semigroup.WrappedMonoid where
    aliftParseJSON p _ a = fmap Semigroup.WrapMonoid <$> p a
    {-# INLINE aliftParseJSON #-}

    aliftParseJSONList _ p a = fmap (fmap Semigroup.WrapMonoid) <$> p a
    {-# INLINE aliftParseJSONList #-}

instance (AFromJSON m a) => AFromJSON m (Semigroup.WrappedMonoid a) where
    aparseJSON = aparseJSON1
    {-# INLINE aparseJSON #-}

    aparseJSONList = aliftParseJSONList aparseJSON aparseJSONList
    {-# INLINE aparseJSONList #-}


instance Applicative m => AFromJSON1 m Semigroup.Option where
    aliftParseJSON p p' = fmap (fmap Semigroup.Option) . aliftParseJSON p p'
    {-# INLINE aliftParseJSON #-}

instance AFromJSON m a => AFromJSON m (Semigroup.Option a) where
    aparseJSON = aparseJSON1
    {-# INLINE aparseJSON #-}

-------------------------------------------------------------------------------
-- tagged
-------------------------------------------------------------------------------

instance Applicative m => AFromJSON1 m Proxy where
    {-# INLINE aliftParseJSON #-}
    aliftParseJSON _ _ Null = pure $ pure Proxy
    aliftParseJSON _ _ v    = typeMismatch "Proxy" v

instance Applicative m => AFromJSON m (Proxy a) where
    {-# INLINE aparseJSON #-}
    aparseJSON Null = pure $ pure Proxy
    aparseJSON v    = typeMismatch "Proxy" v


instance Applicative m => AFromJSON2 m Tagged where
    aliftParseJSON2 _ _ p _ = fmap (fmap Tagged) . p
    {-# INLINE aliftParseJSON2 #-}

instance Applicative m => AFromJSON1 m (Tagged a) where
    aliftParseJSON p _ = fmap (fmap Tagged) . p
    {-# INLINE aliftParseJSON #-}

instance AFromJSON m b => AFromJSON m (Tagged a b) where
    aparseJSON = aparseJSON1
    {-# INLINE aparseJSON #-}

-------------------------------------------------------------------------------
-- Tuple instances, see tuple-instances-from.hs
-------------------------------------------------------------------------------

instance Applicative m => AFromJSON2 m (,) where
    aliftParseJSON2 pA _ pB _ = withArray "(a, b)" $ \t ->
        let n = V.length t
        in if n == 2
            then getCompose $ (,)
                <$> Compose (_parseJSONElemAtIndex pA 0 t)
                <*> Compose (_parseJSONElemAtIndex pB 1 t)
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 2"
    {-# INLINE aliftParseJSON2 #-}

instance (AFromJSON m a) => AFromJSON1 m ((,) a) where
    aliftParseJSON = aliftParseJSON2 aparseJSON aparseJSONList
    {-# INLINE aliftParseJSON #-}

instance (AFromJSON m a, AFromJSON m b) => AFromJSON m (a, b) where
    aparseJSON = aparseJSON2
    {-# INLINE aparseJSON #-}


instance (AFromJSON m a) => AFromJSON2 m ((,,) a) where
    aliftParseJSON2 pB _ pC _ = withArray "(a, b, c)" $ \t ->
        let n = V.length t
        in if n == 3
            then getCompose $ (,,)
                <$> Compose (_parseJSONElemAtIndex aparseJSON 0 t)
                <*> Compose (_parseJSONElemAtIndex pB 1 t)
                <*> Compose (_parseJSONElemAtIndex pC 2 t)
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 3"
    {-# INLINE aliftParseJSON2 #-}

instance (AFromJSON m a, AFromJSON m b) => AFromJSON1 m ((,,) a b) where
    aliftParseJSON = aliftParseJSON2 aparseJSON aparseJSONList
    {-# INLINE aliftParseJSON #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c) => AFromJSON m (a, b, c) where
    aparseJSON = aparseJSON2
    {-# INLINE aparseJSON #-}


instance (AFromJSON m a, AFromJSON m b) => AFromJSON2 m ((,,,) a b) where
    aliftParseJSON2 pC _ pD _ = withArray "(a, b, c, d)" $ \t ->
        let n = V.length t
        in if n == 4
            then getCompose $ (,,,)
                <$> Compose (_parseJSONElemAtIndex aparseJSON 0 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 1 t)
                <*> Compose (_parseJSONElemAtIndex pC 2 t)
                <*> Compose (_parseJSONElemAtIndex pD 3 t)
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 4"
    {-# INLINE aliftParseJSON2 #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c) => AFromJSON1 m ((,,,) a b c) where
    aliftParseJSON = aliftParseJSON2 aparseJSON aparseJSONList
    {-# INLINE aliftParseJSON #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d) => AFromJSON m (a, b, c, d) where
    aparseJSON = aparseJSON2
    {-# INLINE aparseJSON #-}


instance (AFromJSON m a, AFromJSON m b, AFromJSON m c) => AFromJSON2 m ((,,,,) a b c) where
    aliftParseJSON2 pD _ pE _ = withArray "(a, b, c, d, e)" $ \t ->
        let n = V.length t
        in if n == 5
            then getCompose $ (,,,,)
                <$> Compose (_parseJSONElemAtIndex aparseJSON 0 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 1 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 2 t)
                <*> Compose (_parseJSONElemAtIndex pD 3 t)
                <*> Compose (_parseJSONElemAtIndex pE 4 t)
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 5"
    {-# INLINE aliftParseJSON2 #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d) => AFromJSON1 m ((,,,,) a b c d) where
    aliftParseJSON = aliftParseJSON2 aparseJSON aparseJSONList
    {-# INLINE aliftParseJSON #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e) => AFromJSON m (a, b, c, d, e) where
    aparseJSON = aparseJSON2
    {-# INLINE aparseJSON #-}


instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d) => AFromJSON2 m ((,,,,,) a b c d) where
    aliftParseJSON2 pE _ pF _ = withArray "(a, b, c, d, e, f)" $ \t ->
        let n = V.length t
        in if n == 6
            then getCompose $ (,,,,,)
                <$> Compose (_parseJSONElemAtIndex aparseJSON 0 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 1 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 2 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 3 t)
                <*> Compose (_parseJSONElemAtIndex pE 4 t)
                <*> Compose (_parseJSONElemAtIndex pF 5 t)
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 6"
    {-# INLINE aliftParseJSON2 #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e) => AFromJSON1 m ((,,,,,) a b c d e) where
    aliftParseJSON = aliftParseJSON2 aparseJSON aparseJSONList
    {-# INLINE aliftParseJSON #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f) => AFromJSON m (a, b, c, d, e, f) where
    aparseJSON = aparseJSON2
    {-# INLINE aparseJSON #-}


instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e) => AFromJSON2 m ((,,,,,,) a b c d e) where
    aliftParseJSON2 pF _ pG _ = withArray "(a, b, c, d, e, f, g)" $ \t ->
        let n = V.length t
        in if n == 7
            then getCompose $ (,,,,,,)
                <$> Compose (_parseJSONElemAtIndex aparseJSON 0 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 1 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 2 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 3 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 4 t)
                <*> Compose (_parseJSONElemAtIndex pF 5 t)
                <*> Compose (_parseJSONElemAtIndex pG 6 t)
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 7"
    {-# INLINE aliftParseJSON2 #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f) => AFromJSON1 m ((,,,,,,) a b c d e f) where
    aliftParseJSON = aliftParseJSON2 aparseJSON aparseJSONList
    {-# INLINE aliftParseJSON #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f, AFromJSON m g) => AFromJSON m (a, b, c, d, e, f, g) where
    aparseJSON = aparseJSON2
    {-# INLINE aparseJSON #-}


instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f) => AFromJSON2 m ((,,,,,,,) a b c d e f) where
    aliftParseJSON2 pG _ pH _ = withArray "(a, b, c, d, e, f, g, h)" $ \t ->
        let n = V.length t
        in if n == 8
            then getCompose $ (,,,,,,,)
                <$> Compose (_parseJSONElemAtIndex aparseJSON 0 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 1 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 2 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 3 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 4 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 5 t)
                <*> Compose (_parseJSONElemAtIndex pG 6 t)
                <*> Compose (_parseJSONElemAtIndex pH 7 t)
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 8"
    {-# INLINE aliftParseJSON2 #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f, AFromJSON m g) => AFromJSON1 m ((,,,,,,,) a b c d e f g) where
    aliftParseJSON = aliftParseJSON2 aparseJSON aparseJSONList
    {-# INLINE aliftParseJSON #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f, AFromJSON m g, AFromJSON m h) => AFromJSON m (a, b, c, d, e, f, g, h) where
    aparseJSON = aparseJSON2
    {-# INLINE aparseJSON #-}


instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f, AFromJSON m g) => AFromJSON2 m ((,,,,,,,,) a b c d e f g) where
    aliftParseJSON2 pH _ pI _ = withArray "(a, b, c, d, e, f, g, h, i)" $ \t ->
        let n = V.length t
        in if n == 9
            then getCompose $ (,,,,,,,,)
                <$> Compose (_parseJSONElemAtIndex aparseJSON 0 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 1 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 2 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 3 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 4 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 5 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 6 t)
                <*> Compose (_parseJSONElemAtIndex pH 7 t)
                <*> Compose (_parseJSONElemAtIndex pI 8 t)
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 9"
    {-# INLINE aliftParseJSON2 #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f, AFromJSON m g, AFromJSON m h) => AFromJSON1 m ((,,,,,,,,) a b c d e f g h) where
    aliftParseJSON = aliftParseJSON2 aparseJSON aparseJSONList
    {-# INLINE aliftParseJSON #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f, AFromJSON m g, AFromJSON m h, AFromJSON m i) => AFromJSON m (a, b, c, d, e, f, g, h, i) where
    aparseJSON = aparseJSON2
    {-# INLINE aparseJSON #-}


instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f, AFromJSON m g, AFromJSON m h) => AFromJSON2 m ((,,,,,,,,,) a b c d e f g h) where
    aliftParseJSON2 pI _ pJ _ = withArray "(a, b, c, d, e, f, g, h, i, j)" $ \t ->
        let n = V.length t
        in if n == 10
            then getCompose $ (,,,,,,,,,)
                <$> Compose (_parseJSONElemAtIndex aparseJSON 0 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 1 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 2 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 3 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 4 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 5 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 6 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 7 t)
                <*> Compose (_parseJSONElemAtIndex pI 8 t)
                <*> Compose (_parseJSONElemAtIndex pJ 9 t)
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 10"
    {-# INLINE aliftParseJSON2 #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f, AFromJSON m g, AFromJSON m h, AFromJSON m i) => AFromJSON1 m ((,,,,,,,,,) a b c d e f g h i) where
    aliftParseJSON = aliftParseJSON2 aparseJSON aparseJSONList
    {-# INLINE aliftParseJSON #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f, AFromJSON m g, AFromJSON m h, AFromJSON m i, AFromJSON m j) => AFromJSON m (a, b, c, d, e, f, g, h, i, j) where
    aparseJSON = aparseJSON2
    {-# INLINE aparseJSON #-}


instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f, AFromJSON m g, AFromJSON m h, AFromJSON m i) => AFromJSON2 m ((,,,,,,,,,,) a b c d e f g h i) where
    aliftParseJSON2 pJ _ pK _ = withArray "(a, b, c, d, e, f, g, h, i, j, k)" $ \t ->
        let n = V.length t
        in if n == 11
            then getCompose $ (,,,,,,,,,,)
                <$> Compose (_parseJSONElemAtIndex aparseJSON 0 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 1 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 2 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 3 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 4 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 5 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 6 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 7 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 8 t)
                <*> Compose (_parseJSONElemAtIndex pJ 9 t)
                <*> Compose (_parseJSONElemAtIndex pK 10 t)
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 11"
    {-# INLINE aliftParseJSON2 #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f, AFromJSON m g, AFromJSON m h, AFromJSON m i, AFromJSON m j) => AFromJSON1 m ((,,,,,,,,,,) a b c d e f g h i j) where
    aliftParseJSON = aliftParseJSON2 aparseJSON aparseJSONList
    {-# INLINE aliftParseJSON #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f, AFromJSON m g, AFromJSON m h, AFromJSON m i, AFromJSON m j, AFromJSON m k) => AFromJSON m (a, b, c, d, e, f, g, h, i, j, k) where
    aparseJSON = aparseJSON2
    {-# INLINE aparseJSON #-}


instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f, AFromJSON m g, AFromJSON m h, AFromJSON m i, AFromJSON m j) => AFromJSON2 m ((,,,,,,,,,,,) a b c d e f g h i j) where
    aliftParseJSON2 pK _ pL _ = withArray "(a, b, c, d, e, f, g, h, i, j, k, l)" $ \t ->
        let n = V.length t
        in if n == 12
            then getCompose $ (,,,,,,,,,,,)
                <$> Compose (_parseJSONElemAtIndex aparseJSON 0 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 1 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 2 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 3 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 4 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 5 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 6 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 7 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 8 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 9 t)
                <*> Compose (_parseJSONElemAtIndex pK 10 t)
                <*> Compose (_parseJSONElemAtIndex pL 11 t)
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 12"
    {-# INLINE aliftParseJSON2 #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f, AFromJSON m g, AFromJSON m h, AFromJSON m i, AFromJSON m j, AFromJSON m k) => AFromJSON1 m ((,,,,,,,,,,,) a b c d e f g h i j k) where
    aliftParseJSON = aliftParseJSON2 aparseJSON aparseJSONList
    {-# INLINE aliftParseJSON #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f, AFromJSON m g, AFromJSON m h, AFromJSON m i, AFromJSON m j, AFromJSON m k, AFromJSON m l) => AFromJSON m (a, b, c, d, e, f, g, h, i, j, k, l) where
    aparseJSON = aparseJSON2
    {-# INLINE aparseJSON #-}


instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f, AFromJSON m g, AFromJSON m h, AFromJSON m i, AFromJSON m j, AFromJSON m k) => AFromJSON2 m ((,,,,,,,,,,,,) a b c d e f g h i j k) where
    aliftParseJSON2 pL _ pM _ = withArray "(a, b, c, d, e, f, g, h, i, j, k, l, m)" $ \t ->
        let n = V.length t
        in if n == 13
            then getCompose $ (,,,,,,,,,,,,)
                <$> Compose (_parseJSONElemAtIndex aparseJSON 0 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 1 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 2 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 3 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 4 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 5 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 6 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 7 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 8 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 9 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 10 t)
                <*> Compose (_parseJSONElemAtIndex pL 11 t)
                <*> Compose (_parseJSONElemAtIndex pM 12 t)
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 13"
    {-# INLINE aliftParseJSON2 #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f, AFromJSON m g, AFromJSON m h, AFromJSON m i, AFromJSON m j, AFromJSON m k, AFromJSON m l) => AFromJSON1 m ((,,,,,,,,,,,,) a b c d e f g h i j k l) where
    aliftParseJSON = aliftParseJSON2 aparseJSON aparseJSONList
    {-# INLINE aliftParseJSON #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f, AFromJSON m g, AFromJSON m h, AFromJSON m i, AFromJSON m j, AFromJSON m k, AFromJSON m l, AFromJSON m m') => AFromJSON m (a, b, c, d, e, f, g, h, i, j, k, l, m') where
    aparseJSON = aparseJSON2
    {-# INLINE aparseJSON #-}


instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f, AFromJSON m g, AFromJSON m h, AFromJSON m i, AFromJSON m j, AFromJSON m k, AFromJSON m l) => AFromJSON2 m ((,,,,,,,,,,,,,) a b c d e f g h i j k l) where
    aliftParseJSON2 pM _ pN _ = withArray "(a, b, c, d, e, f, g, h, i, j, k, l, m, n)" $ \t ->
        let n = V.length t
        in if n == 14
            then getCompose $ (,,,,,,,,,,,,,)
                <$> Compose (_parseJSONElemAtIndex aparseJSON 0 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 1 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 2 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 3 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 4 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 5 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 6 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 7 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 8 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 9 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 10 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 11 t)
                <*> Compose (_parseJSONElemAtIndex pM 12 t)
                <*> Compose (_parseJSONElemAtIndex pN 13 t)
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 14"
    {-# INLINE aliftParseJSON2 #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f, AFromJSON m g, AFromJSON m h, AFromJSON m i, AFromJSON m j, AFromJSON m k, AFromJSON m l, AFromJSON m m') => AFromJSON1 m ((,,,,,,,,,,,,,) a b c d e f g h i j k l m') where
    aliftParseJSON = aliftParseJSON2 aparseJSON aparseJSONList
    {-# INLINE aliftParseJSON #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f, AFromJSON m g, AFromJSON m h, AFromJSON m i, AFromJSON m j, AFromJSON m k, AFromJSON m l, AFromJSON m m', AFromJSON m n) => AFromJSON m (a, b, c, d, e, f, g, h, i, j, k, l, m', n) where
    aparseJSON = aparseJSON2
    {-# INLINE aparseJSON #-}


instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f, AFromJSON m g, AFromJSON m h, AFromJSON m i, AFromJSON m j, AFromJSON m k, AFromJSON m l, AFromJSON m m') => AFromJSON2 m ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m') where
    aliftParseJSON2 pN _ pO _ = withArray "(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)" $ \t ->
        let n = V.length t
        in if n == 15
            then getCompose $ (,,,,,,,,,,,,,,)
                <$> Compose (_parseJSONElemAtIndex aparseJSON 0 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 1 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 2 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 3 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 4 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 5 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 6 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 7 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 8 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 9 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 10 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 11 t)
                <*> Compose (_parseJSONElemAtIndex aparseJSON 12 t)
                <*> Compose (_parseJSONElemAtIndex pN 13 t)
                <*> Compose (_parseJSONElemAtIndex pO 14 t)
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 15"
    {-# INLINE aliftParseJSON2 #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f, AFromJSON m g, AFromJSON m h, AFromJSON m i, AFromJSON m j, AFromJSON m k, AFromJSON m l, AFromJSON m m', AFromJSON m n) => AFromJSON1 m ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m' n) where
    aliftParseJSON = aliftParseJSON2 aparseJSON aparseJSONList
    {-# INLINE aliftParseJSON #-}

instance (AFromJSON m a, AFromJSON m b, AFromJSON m c, AFromJSON m d, AFromJSON m e, AFromJSON m f, AFromJSON m g, AFromJSON m h, AFromJSON m i, AFromJSON m j, AFromJSON m k, AFromJSON m l, AFromJSON m m', AFromJSON m n, AFromJSON m o) => AFromJSON m (a, b, c, d, e, f, g, h, i, j, k, l, m', n, o) where
    aparseJSON = aparseJSON2
    {-# INLINE aparseJSON #-}
