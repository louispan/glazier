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

module Data.Aeson.Applicative.Internal.FromJSON where

import Prelude.Compat

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
import Control.Applicative ((<|>), Const(..))
import Control.Monad ((<=<), zipWithM)
import Data.Bits (unsafeShiftR)
import Data.Fixed (Fixed, HasResolution)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(..))
import Data.Functor.Sum (Sum(..))
import Data.Hashable (Hashable(..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Proxy (Proxy(..))
import Data.Ratio ((%), Ratio)
import Data.Scientific (Scientific, base10Exponent)
import Data.Tagged (Tagged(..))
import Data.Text (Text, pack, unpack)
import Data.Time (Day, DiffTime, LocalTime, NominalDiffTime, TimeOfDay, UTCTime, ZonedTime)
import Data.Time.Format (parseTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Data.Traversable as Tr (sequence)
import Data.Vector (Vector)
import Data.Version (Version, parseVersion)
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Storable (Storable)
import Foreign.C.Types (CTime (..))
import GHC.Generics
import Numeric.Natural (Natural)
import Text.ParserCombinators.ReadP (readP_to_S)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Attoparsec.ByteString.Char8 as A (endOfInput, parseOnly, scientific)
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HashSet
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
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
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

import qualified GHC.Exts as Exts
import qualified Data.Primitive.Array as PM
import qualified Data.Primitive.Types as PM

#if MIN_VERSION_primitive(0,6,4)
import qualified Data.Primitive.SmallArray as PM
import qualified Data.Primitive.UnliftedArray as PM
import qualified Data.Primitive.PrimArray as PM
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

-- | Lifting of the 'FromJSON' class to binary type constructors.
--
-- Instead of manually writing your 'FromJSON2' instance, "Data.Aeson.TH"
-- provides Template Haskell functions which will derive an instance at compile time.

-- The compiler cannot provide a default generic implementation for 'liftParseJSON2',
-- unlike 'parseJSON' and 'liftParseJSON'.
class Applicative m => MFromJSON2 m f where
    mliftParseJSON2
        :: (Value -> Parser (m a))
        -> (Value -> Parser (m [a]))
        -> (Value -> Parser (m b))
        -> (Value -> Parser (m [b]))
        -> Value -> Parser (m (f a b))
    mliftParseJSONList2
        :: (Value -> Parser (m a))
        -> (Value -> Parser (m [a]))
        -> (Value -> Parser (m b))
        -> (Value -> Parser (m [b]))
        -> Value -> Parser (m [f a b])
    mliftParseJSONList2 fa ga fb gb v = case v of
        Array vals -> fmap (fmap V.toList . sequenceA) $ V.mapM (mliftParseJSON2 fa ga fb gb) vals
        _ -> typeMismatch "[a]" v

-- | Lift the standard 'parseJSON' function through the type constructor.
mparseJSON2 :: (MFromJSON2 m f, MFromJSON m a, MFromJSON m b) => Value -> Parser (m (f a b))
mparseJSON2 = mliftParseJSON2 mparseJSON mparseJSONList mparseJSON mparseJSONList
{-# INLINE mparseJSON2 #-}

-- -------------------------------------------------------------------------------
-- -- List functions
-- -------------------------------------------------------------------------------

-- | Helper function to use with 'liftParseJSON'. See 'Data.Aeson.ToJSON.listEncoding'.
mlistParser :: Applicative m => (Value -> Parser (m a)) -> Value -> Parser (m [a])
mlistParser f (Array xs) = fmap (sequenceA . V.toList) (V.mapM f xs)
mlistParser _ v = typeMismatch "[a]" v
{-# INLINE mlistParser #-}

-------------------------------------------------------------------------------
-- [] instances
-------------------------------------------------------------------------------

instance Applicative m => MFromJSON1 m [] where
    mliftParseJSON _ p' = p'
    {-# INLINE mliftParseJSON #-}

instance MFromJSON m a => MFromJSON m [a] where
    mparseJSON = mparseJSON1

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
mparseFieldMaybe :: (Applicative m, MFromJSON m a) => Object -> Text -> Parser (m (Maybe a))
mparseFieldMaybe = mexplicitParseFieldMaybe mparseJSON
{-# INLINE mparseFieldMaybe #-}

-- | Variant of '.:?' with explicit parser function.
mexplicitParseFieldMaybe :: Applicative m => (Value -> Parser (m a)) -> Object -> Text -> Parser (m (Maybe a))
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

instance ( Applicative m, MConsFromJSON m arity a
         , AllNullary (C1 c a) allNullary
         , MParseSum m arity (C1 c a) allNullary
         ) => MGFromJSON m arity (D1 d (C1 c a)) where
    -- The option 'tagSingleConstructors' determines whether to wrap
    -- a single-constructor type.
    mgParseJSON opts fargs
#if MIN_VERSION_aeson(1,3,0)
        | tagSingleConstructors opts
            = fmap (fmap M1)
            . (unTagged :: Tagged allNullary (Parser (m (C1 c a p))) -> Parser (m (C1 c a p)))
            . mparseSum opts fargs
#endif
        | otherwise = fmap (fmap (M1 . M1)) . mconsParseJSON opts fargs

instance (Applicative m, MConsFromJSON m arity a) => MGFromJSON m arity (C1 c a) where
    -- Constructors need to be decoded differently depending on whether they're
    -- a record or not. This distinction is made by consParseJSON:
    mgParseJSON opts fargs = fmap (fmap M1) . mconsParseJSON opts fargs

instance ( Applicative m, MFromProduct m arity a, MFromProduct m arity b
         , ProductSize a, ProductSize b
         ) => MGFromJSON m arity (a :*: b) where
    -- Products are expected to be encoded to an array. Here we check whether we
    -- got an array of the same size as the product, then parse each of the
    -- product's elements using parseProduct:
    mgParseJSON opts fargs = withArray "product (:*:)" $ \arr ->
      let lenArray = V.length arr
          lenProduct = (unTagged2 :: Tagged2 (a :*: b) Int -> Int)
                       productSize in
      if lenArray == lenProduct
      then mparseProduct opts fargs arr 0 lenProduct
      else fail $ "When expecting a product of " ++ show lenProduct ++
                  " values, encountered an Array of " ++ show lenArray ++
                  " elements instead"

instance ( Applicative m
         , AllNullary (a :+: b) allNullary
         , MParseSum m arity (a :+: b) allNullary
         ) => MGFromJSON m arity (a :+: b) where
    -- If all constructors of a sum datatype are nullary and the
    -- 'allNullaryToStringTag' option is set they are expected to be
    -- encoded as strings.  This distinction is made by 'parseSum':
    mgParseJSON opts fargs =
      (unTagged :: Tagged allNullary (Parser (m ((a :+: b) d))) ->
                                     Parser (m ((a :+: b) d)))
                 . mparseSum opts fargs

instance (MFromJSON1 m f, MGFromJSON m One g) => MGFromJSON m One (f :.: g) where
    -- If an occurrence of the last type parameter is nested inside two
    -- composed types, it is decoded by using the outermost type's FromJSON1
    -- instance to generically decode the innermost type:
    mgParseJSON opts fargs =
      let gpj = mgParseJSON opts fargs in
      fmap (fmap Comp1) . mliftParseJSON gpj (mlistParser gpj)

--------------------------------------------------------------------------------

class MParseSum m arity f allNullary where
    mparseSum :: Options -> MFromArgs m arity a
             -> Value -> Tagged allNullary (Parser (m (f a)))

instance ( MSumFromString m f
         , MFromPair m arity f
         , MFromTaggedObject m arity f
         , MFromUntaggedValue m arity f
         ) => MParseSum m arity f 'True where
    mparseSum opts fargs
        | allNullaryToStringTag opts = Tagged . mparseAllNullarySum opts
        | otherwise                  = Tagged . mparseNonAllNullarySum opts fargs

instance ( MFromPair m arity f
         , MFromTaggedObject m arity f
         , MFromUntaggedValue m arity f
         ) => MParseSum m arity f 'False where
    mparseSum opts fargs = Tagged . mparseNonAllNullarySum opts fargs

--------------------------------------------------------------------------------

mparseAllNullarySum :: MSumFromString m f => Options -> Value -> Parser (m (f a))
mparseAllNullarySum opts = withText "Text" $ \key ->
                            maybe (notFound key) pure $
                              mparseSumFromString opts key

class MSumFromString m f where
    mparseSumFromString :: Options -> Text -> Maybe (m (f a))

instance (Functor m, MSumFromString m a, MSumFromString m b) => MSumFromString m (a :+: b) where
    mparseSumFromString opts key = (fmap L1 <$> mparseSumFromString opts key) <|>
                                  (fmap R1 <$> mparseSumFromString opts key)

instance (Applicative m, Constructor c) => MSumFromString m (C1 c U1) where
    mparseSumFromString opts key | key == name = Just $ pure $ M1 U1
                                 | otherwise   = Nothing
        where
          name = pack $ constructorTagModifier opts $
                          conName (undefined :: t c U1 p)

--------------------------------------------------------------------------------

mparseNonAllNullarySum :: ( MFromPair m arity f
                         , MFromTaggedObject m arity f
                         , MFromUntaggedValue m arity f
                         ) => Options -> MFromArgs m arity c
                           -> Value -> Parser (m (f c))
mparseNonAllNullarySum opts fargs =
    case sumEncoding opts of
      TaggedObject{..} ->
          withObject "Object" $ \obj -> do
            tag <- obj .: pack tagFieldName
            fromMaybe (notFound tag) $
              mparseFromTaggedObject opts fargs contentsFieldName obj tag

      ObjectWithSingleField ->
          withObject "Object" $ \obj ->
            case H.toList obj of
              [pr@(tag, _)] -> fromMaybe (notFound tag) $
                                   mparsePair opts fargs pr
              _ -> fail "Object doesn't have a single field"

      TwoElemArray ->
          withArray "Array" $ \arr ->
            if V.length arr == 2
            then case V.unsafeIndex arr 0 of
                   String tag -> fromMaybe (notFound tag) $
                                   mparsePair opts fargs (tag, V.unsafeIndex arr 1)
                   _ -> fail "First element is not a String"
            else fail "Array doesn't have 2 elements"

      UntaggedValue -> mparseUntaggedValue opts fargs

--------------------------------------------------------------------------------

class MFromTaggedObject m arity f where
    mparseFromTaggedObject :: Options -> MFromArgs m arity a
                          -> String -> Object
                          -> Text -> Maybe (Parser (m (f a)))

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
          name = pack $ constructorTagModifier opts $
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

instance (MFromRecord m arity f) => MFromTaggedObject'' m arity f 'True where
    mparseFromTaggedObject'' opts fargs _ =
      Tagged . mparseRecord opts fargs

instance (MGFromJSON m arity f) => MFromTaggedObject'' m arity f 'False where
    mparseFromTaggedObject'' opts fargs contentsFieldName = Tagged .
      (mgParseJSON opts fargs <=< (.: pack contentsFieldName))

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

instance MFromRecord m arity f => MConsFromJSON' m arity f 'True where
    mconsParseJSON' opts fargs =
      Tagged . withObject "record (:*:)" (mparseRecord opts fargs)

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
          label = pack . fieldLabelModifier opts $ selName (undefined :: t s a p)

instance {-# INCOHERENT #-} (Selector s, MFromJSON m a) =>
  MFromRecord m arity (S1 s (K1 i (Maybe a))) where
    mparseRecord opts _ obj = fmap (M1 . K1) <$> obj `mparseFieldMaybe` pack label
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

class MFromProduct m arity f where
    mparseProduct :: Options -> MFromArgs m arity a
                 -> Array -> Int -> Int
                 -> Parser (m (f a))

instance ( Applicative m
         , MFromProduct m arity a
         , MFromProduct m arity b
         ) => MFromProduct m arity (a :*: b) where
    mparseProduct opts fargs arr ix len = getCompose $
        (:*:) <$> Compose (mparseProduct opts fargs arr ix  lenL)
              <*> Compose (mparseProduct opts fargs arr ixR lenR)
        where
          lenL = len `unsafeShiftR` 1
          ixR  = ix + lenL
          lenR = len - lenL

instance (MGFromJSON m arity a) => MFromProduct m arity (S1 s a) where
    mparseProduct opts fargs arr ix _ =
      mgParseJSON opts fargs $ V.unsafeIndex arr ix

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

instance ( Constructor c
         , MGFromJSON m arity a
         , MConsFromJSON m arity a
         ) => MFromPair m arity (C1 c a) where
    mparsePair opts fargs (tag, value)
        | tag == tag' = Just $ mgParseJSON opts fargs value
        | otherwise   = Nothing
        where
          tag' = pack $ constructorTagModifier opts $
                          conName (undefined :: t c a p)

--------------------------------------------------------------------------------

class MFromUntaggedValue m arity f where
    mparseUntaggedValue :: Options -> MFromArgs m arity a
                       -> Value -> Parser (m (f a))

instance
    ( Functor m
    , MFromUntaggedValue m arity a
    , MFromUntaggedValue m arity b
    ) => MFromUntaggedValue m arity (a :+: b)
  where
    mparseUntaggedValue opts fargs value =
        fmap L1 <$> mparseUntaggedValue opts fargs value <|>
        fmap R1 <$> mparseUntaggedValue opts fargs value

instance {-# OVERLAPPABLE #-}
    ( MGFromJSON m arity a
    , MConsFromJSON m arity a
    ) => MFromUntaggedValue m arity (C1 c a)
  where
    mparseUntaggedValue = mgParseJSON

instance {-# OVERLAPPING #-}
    ( Applicative m, Constructor c )
    => MFromUntaggedValue m arity (C1 c U1)
  where
    mparseUntaggedValue opts _ (String s)
        | s == pack (constructorTagModifier opts (conName (undefined :: t c U1 p))) =
            pure $ pure $ M1 U1
        | otherwise =
            fail $ "Invalid tag: " ++ unpack s
    mparseUntaggedValue _ _ v = typeMismatch (conName (undefined :: t c U1 p)) v

--------------------------------------------------------------------------------

notFound :: Text -> Parser a
notFound key = fail $ "The key \"" ++ unpack key ++ "\" was not found"
{-# INLINE notFound #-}


-- -------------------------------------------------------------------------------
-- -- Instances
-- -------------------------------------------------------------------------------

-- -------------------------------------------------------------------------------
-- -- base
-- -------------------------------------------------------------------------------


-- instance MFromJSON2 m Const where
--     liftParseJSON2 p _ _ _ = fmap Const . p
--     {-# INLINE liftParseJSON2 #-}

-- instance MFromJSON m a => MFromJSON1 m (Const a) where
--     liftParseJSON _ _ = fmap Const . parseJSON
--     {-# INLINE liftParseJSON #-}

-- instance MFromJSON m a => MFromJSON m (Const a b) where
--     {-# INLINE parseJSON #-}
--     parseJSON = fmap Const . parseJSON


instance Applicative m => MFromJSON1 m Maybe where
    mliftParseJSON _ _ Null = pure (pure Nothing)
    mliftParseJSON p _ a    = fmap Just <$> p a
    {-# INLINE mliftParseJSON #-}

instance (MFromJSON m  a) => MFromJSON m (Maybe a) where
    mparseJSON = mparseJSON1
    {-# INLINE mparseJSON #-}



-- instance MFromJSON2 m Either where
--     liftParseJSON2 pA _ pB _ (Object (H.toList -> [(key, value)]))
--         | key == left  = Left  <$> pA value <?> Key left
--         | key == right = Right <$> pB value <?> Key right
--       where
--         left, right :: Text
--         left  = "Left"
--         right = "Right"

--     liftParseJSON2 _ _ _ _ _ = fail $
--         "expected an object with a single property " ++
--         "where the property key should be either " ++
--         "\"Left\" or \"Right\""
--     {-# INLINE liftParseJSON2 #-}

-- instance (MFromJSON m  a) => MFromJSON1 m (Either a) where
--     liftParseJSON = liftParseJSON2 parseJSON parseJSONList
--     {-# INLINE liftParseJSON #-}

-- instance (MFromJSON m  a, MFromJSON m b) => MFromJSON m (Either a b) where
--     parseJSON = parseJSON2
--     {-# INLINE parseJSON #-}

-- instance MFromJSON m Void where
--     parseJSON _ = fail "Cannot parse Void"
--     {-# INLINE parseJSON #-}

-- instance MFromJSON m Bool where
--     parseJSON = withBool "Bool" pure
--     {-# INLINE parseJSON #-}

-- instance FromJSONKey Bool where
--     fromJSONKey = FromJSONKeyTextParser $ \t -> case t of
--         "true"  -> pure True
--         "false" -> pure False
--         _       -> fail $ "Cannot parse key into Bool: " ++ T.unpack t

-- instance MFromJSON m Ordering where
--   parseJSON = withText "Ordering" $ \s ->
--     case s of
--       "LT" -> return LT
--       "EQ" -> return EQ
--       "GT" -> return GT
--       _ -> fail "Parsing Ordering value failed: expected \"LT\", \"EQ\", or \"GT\""

-- instance MFromJSON m () where
--     parseJSON = withArray "()" $ \v ->
--                   if V.null v
--                     then pure ()
--                     else fail "Expected an empty array"
--     {-# INLINE parseJSON #-}

-- instance MFromJSON m Char where
--     parseJSON = withText "Char" $ \t ->
--                   if T.compareLength t 1 == EQ
--                     then pure $ T.head t
--                     else fail "Expected a string of length 1"
--     {-# INLINE parseJSON #-}

--     parseJSONList = withText "String" $ pure . T.unpack
--     {-# INLINE parseJSONList #-}

-- instance MFromJSON m Double where
--     parseJSON = parseRealFloat "Double"
--     {-# INLINE parseJSON #-}

-- instance FromJSONKey Double where
--     fromJSONKey = FromJSONKeyTextParser $ \t -> case t of
--         "NaN"       -> pure (0/0)
--         "Infinity"  -> pure (1/0)
--         "-Infinity" -> pure (negate 1/0)
--         _           -> Scientific.toRealFloat <$> parseScientificText t

-- instance MFromJSON m Float where
--     parseJSON = parseRealFloat "Float"
--     {-# INLINE parseJSON #-}

-- instance FromJSONKey Float where
--     fromJSONKey = FromJSONKeyTextParser $ \t -> case t of
--         "NaN"       -> pure (0/0)
--         "Infinity"  -> pure (1/0)
--         "-Infinity" -> pure (negate 1/0)
--         _           -> Scientific.toRealFloat <$> parseScientificText t

-- instance (MFromJSON m  a, Integral a) => MFromJSON m (Ratio a) where
--     parseJSON = withObject "Rational" $ \obj -> do
--         numerator <- obj .: "numerator"
--         denominator <- obj .: "denominator"
--         if denominator == 0
--         then fail "Ratio denominator was 0"
--         else pure $ numerator % denominator
--     {-# INLINE parseJSON #-}

-- -- | This instance includes a bounds check to prevent maliciously
-- -- large inputs to fill up the memory of the target system. You can
-- -- newtype 'Scientific' and provide your own instance using
-- -- 'withScientific' if you want to allow larger inputs.
-- instance HasResolution a => MFromJSON m (Fixed a) where
--     parseJSON = withBoundedScientific "Fixed" $ pure . realToFrac
--     {-# INLINE parseJSON #-}

-- instance MFromJSON m Int where
--     parseJSON = parseBoundedIntegral "Int"
--     {-# INLINE parseJSON #-}

-- instance FromJSONKey Int where
--     fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Int"

-- -- | This instance includes a bounds check to prevent maliciously
-- -- large inputs to fill up the memory of the target system. You can
-- -- newtype 'Scientific' and provide your own instance using
-- -- 'withScientific' if you want to allow larger inputs.
-- instance MFromJSON m Integer where
--     parseJSON = parseIntegral "Integer"
--     {-# INLINE parseJSON #-}

-- instance FromJSONKey Integer where
--     fromJSONKey = FromJSONKeyTextParser $ parseIntegralText "Integer"

-- instance MFromJSON m Natural where
--     parseJSON value = do
--         integer :: Integer <- parseIntegral "Natural" value
--         if integer < 0 then
--             fail $ "expected Natural, encountered negative number " <> show integer
--         else
--             pure $ fromIntegral integer

-- instance FromJSONKey Natural where
--     fromJSONKey = FromJSONKeyTextParser $ \text -> do
--         integer :: Integer <- parseIntegralText "Natural" text
--         if integer < 0 then
--             fail $ "expected Natural, encountered negative number " <> show integer
--         else
--             pure $ fromIntegral integer

-- instance MFromJSON m Int8 where
--     parseJSON = parseBoundedIntegral "Int8"
--     {-# INLINE parseJSON #-}

-- instance FromJSONKey Int8 where
--     fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Int8"

-- instance MFromJSON m Int16 where
--     parseJSON = parseBoundedIntegral "Int16"
--     {-# INLINE parseJSON #-}

-- instance FromJSONKey Int16 where
--     fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Int16"

-- instance MFromJSON m Int32 where
--     parseJSON = parseBoundedIntegral "Int32"
--     {-# INLINE parseJSON #-}

-- instance FromJSONKey Int32 where
--     fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Int32"

-- instance MFromJSON m Int64 where
--     parseJSON = parseBoundedIntegral "Int64"
--     {-# INLINE parseJSON #-}

-- instance FromJSONKey Int64 where
--     fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Int64"

-- instance MFromJSON m Word where
--     parseJSON = parseBoundedIntegral "Word"
--     {-# INLINE parseJSON #-}

-- instance FromJSONKey Word where
--     fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Word"

-- instance MFromJSON m Word8 where
--     parseJSON = parseBoundedIntegral "Word8"
--     {-# INLINE parseJSON #-}

-- instance FromJSONKey Word8 where
--     fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Word8"

-- instance MFromJSON m Word16 where
--     parseJSON = parseBoundedIntegral "Word16"
--     {-# INLINE parseJSON #-}

-- instance FromJSONKey Word16 where
--     fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Word16"

-- instance MFromJSON m Word32 where
--     parseJSON = parseBoundedIntegral "Word32"
--     {-# INLINE parseJSON #-}

-- instance FromJSONKey Word32 where
--     fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Word32"

-- instance MFromJSON m Word64 where
--     parseJSON = parseBoundedIntegral "Word64"
--     {-# INLINE parseJSON #-}

-- instance FromJSONKey Word64 where
--     fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Word64"

-- instance MFromJSON m CTime where
--     parseJSON = fmap CTime . parseJSON
--     {-# INLINE parseJSON #-}

-- instance MFromJSON m Text where
--     parseJSON = withText "Text" pure
--     {-# INLINE parseJSON #-}

-- instance FromJSONKey Text where
--     fromJSONKey = fromJSONKeyCoerce


-- instance MFromJSON m LT.Text where
--     parseJSON = withText "Lazy Text" $ pure . LT.fromStrict
--     {-# INLINE parseJSON #-}

-- instance FromJSONKey LT.Text where
--     fromJSONKey = FromJSONKeyText LT.fromStrict


-- instance MFromJSON m Version where
--     parseJSON = withText "Version" parseVersionText
--     {-# INLINE parseJSON #-}

-- instance FromJSONKey Version where
--     fromJSONKey = FromJSONKeyTextParser parseVersionText

-- parseVersionText :: Text -> Parser Version
-- parseVersionText = go . readP_to_S parseVersion . unpack
--   where
--     go [(v,[])] = return v
--     go (_ : xs) = go xs
--     go _        = fail "could not parse Version"

-- -------------------------------------------------------------------------------
-- -- semigroups NonEmpty
-- -------------------------------------------------------------------------------

-- instance MFromJSON1 m NonEmpty where
--     liftParseJSON p _ = withArray "NonEmpty a" $
--         (>>= ne) . Tr.sequence . zipWith (parseIndexedJSON p) [0..] . V.toList
--       where
--         ne []     = fail "Expected a NonEmpty but got an empty list"
--         ne (x:xs) = pure (x :| xs)
--     {-# INLINE liftParseJSON #-}

-- instance (MFromJSON m  a) => MFromJSON m (NonEmpty a) where
--     parseJSON = parseJSON1
--     {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------------
-- -- scientific
-- -------------------------------------------------------------------------------

-- instance MFromJSON m Scientific where
--     parseJSON = withScientific "Scientific" pure
--     {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------------
-- -- DList
-- -------------------------------------------------------------------------------

-- instance MFromJSON1 m DList.DList where
--     liftParseJSON p _ = withArray "DList a" $
--       fmap DList.fromList .
--       Tr.sequence . zipWith (parseIndexedJSON p) [0..] . V.toList
--     {-# INLINE liftParseJSON #-}

-- instance (MFromJSON m  a) => MFromJSON m (DList.DList a) where
--     parseJSON = parseJSON1
--     {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------------
-- -- tranformers - Functors
-- -------------------------------------------------------------------------------

-- instance MFromJSON1 m Identity where
--     liftParseJSON p _ a = Identity <$> p a
--     {-# INLINE liftParseJSON #-}

--     liftParseJSONList _ p a = fmap Identity <$> p a
--     {-# INLINE liftParseJSONList #-}

-- instance (MFromJSON m  a) => MFromJSON m (Identity a) where
--     parseJSON = parseJSON1
--     {-# INLINE parseJSON #-}

--     parseJSONList = liftParseJSONList parseJSON parseJSONList
--     {-# INLINE parseJSONList #-}

-- instance (MFromJSON m Key a) => FromJSONKey (Identity a) where
--     fromJSONKey = coerceFromJSONKeyFunction (fromJSONKey :: FromJSONKeyFunction a)
--     fromJSONKeyList = coerceFromJSONKeyFunction (fromJSONKeyList :: FromJSONKeyFunction [a])


-- instance (MFromJSON1 m  f, MFromJSON1 m g) => MFromJSON1 m (Compose f g) where
--     liftParseJSON p pl a = Compose <$> liftParseJSON g gl a
--       where
--         g  = liftParseJSON p pl
--         gl = liftParseJSONList p pl
--     {-# INLINE liftParseJSON #-}

--     liftParseJSONList p pl a = map Compose <$> liftParseJSONList g gl a
--       where
--         g  = liftParseJSON p pl
--         gl = liftParseJSONList p pl
--     {-# INLINE liftParseJSONList #-}

-- instance (MFromJSON1 m  f, MFromJSON1 m g, MFromJSON m a) => MFromJSON m (Compose f g a) where
--     parseJSON = parseJSON1
--     {-# INLINE parseJSON #-}

--     parseJSONList = liftParseJSONList parseJSON parseJSONList
--     {-# INLINE parseJSONList #-}


-- instance (MFromJSON1 m  f, MFromJSON1 m g) => MFromJSON1 m (Product f g) where
--     liftParseJSON p pl a = uncurry Pair <$> liftParseJSON2 px pxl py pyl a
--       where
--         px  = liftParseJSON p pl
--         pxl = liftParseJSONList p pl
--         py  = liftParseJSON p pl
--         pyl = liftParseJSONList p pl
--     {-# INLINE liftParseJSON #-}

-- instance (MFromJSON1 m  f, MFromJSON1 m g, MFromJSON m a) => MFromJSON m (Product f g a) where
--     parseJSON = parseJSON1
--     {-# INLINE parseJSON #-}


-- instance (MFromJSON1 m  f, MFromJSON1 m g) => MFromJSON1 m (Sum f g) where
--     liftParseJSON p pl (Object (H.toList -> [(key, value)]))
--         | key == inl = InL <$> liftParseJSON p pl value <?> Key inl
--         | key == inr = InR <$> liftParseJSON p pl value <?> Key inl
--       where
--         inl, inr :: Text
--         inl = "InL"
--         inr = "InR"

--     liftParseJSON _ _ _ = fail $
--         "expected an object with a single property " ++
--         "where the property key should be either " ++
--         "\"InL\" or \"InR\""
--     {-# INLINE liftParseJSON #-}

-- instance (MFromJSON1 m  f, MFromJSON1 m g, MFromJSON m a) => MFromJSON m (Sum f g a) where
--     parseJSON = parseJSON1
--     {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------------
-- -- containers
-- -------------------------------------------------------------------------------

-- instance MFromJSON1 m Seq.Seq where
--     liftParseJSON p _ = withArray "Seq a" $
--       fmap Seq.fromList .
--       Tr.sequence . zipWith (parseIndexedJSON p) [0..] . V.toList
--     {-# INLINE liftParseJSON #-}

-- instance (MFromJSON m  a) => MFromJSON m (Seq.Seq a) where
--     parseJSON = parseJSON1
--     {-# INLINE parseJSON #-}


-- instance (Ord a, MFromJSON m a) => MFromJSON m (Set.Set a) where
--     parseJSON = fmap Set.fromList . parseJSON
--     {-# INLINE parseJSON #-}


-- instance MFromJSON m IntSet.IntSet where
--     parseJSON = fmap IntSet.fromList . parseJSON
--     {-# INLINE parseJSON #-}


-- instance MFromJSON1 m IntMap.IntMap where
--     liftParseJSON p pl = fmap IntMap.fromList . liftParseJSON p' pl'
--       where
--         p'  = liftParseJSON2     parseJSON parseJSONList p pl
--         pl' = liftParseJSONList2 parseJSON parseJSONList p pl
--     {-# INLINE liftParseJSON #-}

-- instance MFromJSON m a => MFromJSON m (IntMap.IntMap a) where
--     parseJSON = fmap IntMap.fromList . parseJSON
--     {-# INLINE parseJSON #-}


-- instance (MFromJSON m Key k, Ord k) => MFromJSON1 m (M.Map k) where
--     liftParseJSON p _ = case fromJSONKey of
--         FromJSONKeyCoerce _-> withObject "Map k v" $
--             fmap (H.foldrWithKey (M.insert . unsafeCoerce) M.empty) . H.traverseWithKey (\k v -> p v <?> Key k)
--         FromJSONKeyText f -> withObject "Map k v" $
--             fmap (H.foldrWithKey (M.insert . f) M.empty) . H.traverseWithKey (\k v -> p v <?> Key k)
--         FromJSONKeyTextParser f -> withObject "Map k v" $
--             H.foldrWithKey (\k v m -> M.insert <$> f k <?> Key k <*> p v <?> Key k <*> m) (pure M.empty)
--         FromJSONKeyValue f -> withArray "Map k v" $ \arr ->
--             fmap M.fromList . Tr.sequence .
--                 zipWith (parseIndexedJSONPair f p) [0..] . V.toList $ arr
--     {-# INLINE liftParseJSON #-}

-- instance (MFromJSON m Key k, Ord k, MFromJSON m v) => MFromJSON m (M.Map k v) where
--     parseJSON = parseJSON1
--     {-# INLINE parseJSON #-}


-- instance MFromJSON1 m Tree.Tree where
--     liftParseJSON p pl = go
--       where
--         go v = uncurry Tree.Node <$> liftParseJSON2 p pl p' pl' v

--         p' = liftParseJSON go (listParser go)
--         pl'= liftParseJSONList go (listParser go)

-- instance (MFromJSON m  v) => MFromJSON m (Tree.Tree v) where
--     parseJSON = parseJSON1
--     {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------------
-- -- uuid
-- -------------------------------------------------------------------------------

-- instance MFromJSON m UUID.UUID where
--     parseJSON = withText "UUID" $
--         maybe (fail "Invalid UUID") pure . UUID.fromText

-- instance FromJSONKey UUID.UUID where
--     fromJSONKey = FromJSONKeyTextParser $
--         maybe (fail "Invalid UUID") pure . UUID.fromText

-- -------------------------------------------------------------------------------
-- -- vector
-- -------------------------------------------------------------------------------

-- instance MFromJSON1 m Vector where
--     liftParseJSON p _ = withArray "Vector a" $
--         V.mapM (uncurry $ parseIndexedJSON p) . V.indexed
--     {-# INLINE liftParseJSON #-}

-- instance (MFromJSON m  a) => MFromJSON m (Vector a) where
--     parseJSON = parseJSON1
--     {-# INLINE parseJSON #-}


-- vectorParseJSON :: (MFromJSON m  a, VG.Vector w a) => String -> Value -> Parser (w a)
-- vectorParseJSON s = withArray s $ fmap V.convert . V.mapM (uncurry $ parseIndexedJSON parseJSON) . V.indexed
-- {-# INLINE vectorParseJSON #-}

-- instance (Storable a, MFromJSON m a) => MFromJSON m (VS.Vector a) where
--     parseJSON = vectorParseJSON "Data.Vector.Storable.Vector a"

-- instance (VP.Prim a, MFromJSON m a) => MFromJSON m (VP.Vector a) where
--     parseJSON = vectorParseJSON "Data.Vector.Primitive.Vector a"
--     {-# INLINE parseJSON #-}

-- instance (VG.Vector VU.Vector a, MFromJSON m a) => MFromJSON m (VU.Vector a) where
--     parseJSON = vectorParseJSON "Data.Vector.Unboxed.Vector a"
--     {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------------
-- -- unordered-containers
-- -------------------------------------------------------------------------------

-- instance (Eq a, Hashable a, MFromJSON m a) => MFromJSON m (HashSet.HashSet a) where
--     parseJSON = fmap HashSet.fromList . parseJSON
--     {-# INLINE parseJSON #-}


-- instance (MFromJSON m Key k, Eq k, Hashable k) => MFromJSON1 m (H.HashMap k) where
--     liftParseJSON p _ = case fromJSONKey of
--         FromJSONKeyCoerce _ -> withObject "HashMap ~Text v" $
--             uc . H.traverseWithKey (\k v -> p v <?> Key k)
--         FromJSONKeyText f -> withObject "HashMap k v" $
--             fmap (mapKey f) . H.traverseWithKey (\k v -> p v <?> Key k)
--         FromJSONKeyTextParser f -> withObject "HashMap k v" $
--             H.foldrWithKey (\k v m -> H.insert <$> f k <?> Key k <*> p v <?> Key k <*> m) (pure H.empty)
--         FromJSONKeyValue f -> withArray "Map k v" $ \arr ->
--             fmap H.fromList . Tr.sequence .
--                 zipWith (parseIndexedJSONPair f p) [0..] . V.toList $ arr
--       where
--         uc :: Parser (H.HashMap Text v) -> Parser (H.HashMap k v)
--         uc = unsafeCoerce

-- instance (MFromJSON m  v, FromJSONKey k, Eq k, Hashable k) => MFromJSON m (H.HashMap k v) where
--     parseJSON = parseJSON1
--     {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------------
-- -- aeson
-- -------------------------------------------------------------------------------

-- instance MFromJSON m Value where
--     parseJSON = pure
--     {-# INLINE parseJSON #-}

-- instance MFromJSON m DotNetTime where
--     parseJSON = withText "DotNetTime" $ \t ->
--         let (s,m) = T.splitAt (T.length t - 5) t
--             t'    = T.concat [s,".",m]
--         in case parseTime defaultTimeLocale "/Date(%s%Q)/" (unpack t') of
--              Just d -> pure (DotNetTime d)
--              _      -> fail "could not parse .NET time"
--     {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------------
-- -- primitive
-- -------------------------------------------------------------------------------

-- #if MIN_VERSION_base(4,7,0)
-- instance MFromJSON m a => MFromJSON m (PM.Array a) where
--   -- note: we could do better than this if vector exposed the data
--   -- constructor in Data.Vector.
--   parseJSON = fmap Exts.fromList . parseJSON

-- instance MFromJSON m a => MFromJSON m (PM.SmallArray a) where
--   parseJSON = fmap Exts.fromList . parseJSON

-- #if MIN_VERSION_primitive(0,6,4)
-- instance (PM.Prim a,FromJSON a) => MFromJSON m (PM.PrimArray a) where
--   parseJSON = fmap Exts.fromList . parseJSON

-- instance (PM.PrimUnlifted a,FromJSON a) => MFromJSON m (PM.UnliftedArray a) where
--   parseJSON = fmap Exts.fromList . parseJSON
-- #endif
-- #endif

-- -------------------------------------------------------------------------------
-- -- time
-- -------------------------------------------------------------------------------

-- instance MFromJSON m Day where
--     parseJSON = withText "Day" (Time.run Time.day)

-- instance FromJSONKey Day where
--     fromJSONKey = FromJSONKeyTextParser (Time.run Time.day)


-- instance MFromJSON m TimeOfDay where
--     parseJSON = withText "TimeOfDay" (Time.run Time.timeOfDay)

-- instance FromJSONKey TimeOfDay where
--     fromJSONKey = FromJSONKeyTextParser (Time.run Time.timeOfDay)


-- instance MFromJSON m LocalTime where
--     parseJSON = withText "LocalTime" (Time.run Time.localTime)

-- instance FromJSONKey LocalTime where
--     fromJSONKey = FromJSONKeyTextParser (Time.run Time.localTime)


-- -- | Supported string formats:
-- --
-- -- @YYYY-MM-DD HH:MM Z@
-- -- @YYYY-MM-DD HH:MM:SS Z@
-- -- @YYYY-MM-DD HH:MM:SS.SSS Z@
-- --
-- -- The first space may instead be a @T@, and the second space is
-- -- optional.  The @Z@ represents UTC.  The @Z@ may be replaced with a
-- -- time zone offset of the form @+0000@ or @-08:00@, where the first
-- -- two digits are hours, the @:@ is optional and the second two digits
-- -- (also optional) are minutes.
-- instance MFromJSON m ZonedTime where
--     parseJSON = withText "ZonedTime" (Time.run Time.zonedTime)

-- instance FromJSONKey ZonedTime where
--     fromJSONKey = FromJSONKeyTextParser (Time.run Time.zonedTime)


-- instance MFromJSON m UTCTime where
--     parseJSON = withText "UTCTime" (Time.run Time.utcTime)

-- instance FromJSONKey UTCTime where
--     fromJSONKey = FromJSONKeyTextParser (Time.run Time.utcTime)


-- -- | This instance includes a bounds check to prevent maliciously
-- -- large inputs to fill up the memory of the target system. You can
-- -- newtype 'Scientific' and provide your own instance using
-- -- 'withScientific' if you want to allow larger inputs.
-- instance MFromJSON m NominalDiffTime where
--     parseJSON = withBoundedScientific "NominalDiffTime" $ pure . realToFrac
--     {-# INLINE parseJSON #-}


-- -- | This instance includes a bounds check to prevent maliciously
-- -- large inputs to fill up the memory of the target system. You can
-- -- newtype 'Scientific' and provide your own instance using
-- -- 'withScientific' if you want to allow larger inputs.
-- instance MFromJSON m DiffTime where
--     parseJSON = withBoundedScientific "DiffTime" $ pure . realToFrac
--     {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------------
-- -- base Monoid/Semigroup
-- -------------------------------------------------------------------------------

-- instance MFromJSON1 m Monoid.Dual where
--     liftParseJSON p _ = fmap Monoid.Dual . p
--     {-# INLINE liftParseJSON #-}

-- instance MFromJSON m a => MFromJSON m (Monoid.Dual a) where
--     parseJSON = parseJSON1
--     {-# INLINE parseJSON #-}


-- instance MFromJSON1 m Monoid.First where
--     liftParseJSON p p' = fmap Monoid.First . liftParseJSON p p'
--     {-# INLINE liftParseJSON #-}

-- instance MFromJSON m a => MFromJSON m (Monoid.First a) where
--     parseJSON = parseJSON1
--     {-# INLINE parseJSON #-}


-- instance MFromJSON1 m Monoid.Last where
--     liftParseJSON p p' = fmap Monoid.Last . liftParseJSON p p'
--     {-# INLINE liftParseJSON #-}

-- instance MFromJSON m a => MFromJSON m (Monoid.Last a) where
--     parseJSON = parseJSON1
--     {-# INLINE parseJSON #-}


-- instance MFromJSON1 m Semigroup.Min where
--     liftParseJSON p _ a = Semigroup.Min <$> p a
--     {-# INLINE liftParseJSON #-}

--     liftParseJSONList _ p a = fmap Semigroup.Min <$> p a
--     {-# INLINE liftParseJSONList #-}

-- instance (MFromJSON m  a) => MFromJSON m (Semigroup.Min a) where
--     parseJSON = parseJSON1
--     {-# INLINE parseJSON #-}

--     parseJSONList = liftParseJSONList parseJSON parseJSONList
--     {-# INLINE parseJSONList #-}


-- instance MFromJSON1 m Semigroup.Max where
--     liftParseJSON p _ a = Semigroup.Max <$> p a
--     {-# INLINE liftParseJSON #-}

--     liftParseJSONList _ p a = fmap Semigroup.Max <$> p a
--     {-# INLINE liftParseJSONList #-}

-- instance (MFromJSON m  a) => MFromJSON m (Semigroup.Max a) where
--     parseJSON = parseJSON1
--     {-# INLINE parseJSON #-}

--     parseJSONList = liftParseJSONList parseJSON parseJSONList
--     {-# INLINE parseJSONList #-}


-- instance MFromJSON1 m Semigroup.First where
--     liftParseJSON p _ a = Semigroup.First <$> p a
--     {-# INLINE liftParseJSON #-}

--     liftParseJSONList _ p a = fmap Semigroup.First <$> p a
--     {-# INLINE liftParseJSONList #-}

-- instance (MFromJSON m  a) => MFromJSON m (Semigroup.First a) where
--     parseJSON = parseJSON1
--     {-# INLINE parseJSON #-}

--     parseJSONList = liftParseJSONList parseJSON parseJSONList
--     {-# INLINE parseJSONList #-}


-- instance MFromJSON1 m Semigroup.Last where
--     liftParseJSON p _ a = Semigroup.Last <$> p a
--     {-# INLINE liftParseJSON #-}

--     liftParseJSONList _ p a = fmap Semigroup.Last <$> p a
--     {-# INLINE liftParseJSONList #-}

-- instance (MFromJSON m  a) => MFromJSON m (Semigroup.Last a) where
--     parseJSON = parseJSON1
--     {-# INLINE parseJSON #-}

--     parseJSONList = liftParseJSONList parseJSON parseJSONList
--     {-# INLINE parseJSONList #-}


-- instance MFromJSON1 m Semigroup.WrappedMonoid where
--     liftParseJSON p _ a = Semigroup.WrapMonoid <$> p a
--     {-# INLINE liftParseJSON #-}

--     liftParseJSONList _ p a = fmap Semigroup.WrapMonoid <$> p a
--     {-# INLINE liftParseJSONList #-}

-- instance (MFromJSON m  a) => MFromJSON m (Semigroup.WrappedMonoid a) where
--     parseJSON = parseJSON1
--     {-# INLINE parseJSON #-}

--     parseJSONList = liftParseJSONList parseJSON parseJSONList
--     {-# INLINE parseJSONList #-}


-- instance MFromJSON1 m Semigroup.Option where
--     liftParseJSON p p' = fmap Semigroup.Option . liftParseJSON p p'
--     {-# INLINE liftParseJSON #-}

-- instance MFromJSON m a => MFromJSON m (Semigroup.Option a) where
--     parseJSON = parseJSON1
--     {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------------
-- -- tagged
-- -------------------------------------------------------------------------------

-- instance MFromJSON1 m Proxy where
--     {-# INLINE liftParseJSON #-}
--     liftParseJSON _ _ Null = pure Proxy
--     liftParseJSON _ _ v    = typeMismatch "Proxy" v

-- instance MFromJSON m (Proxy a) where
--     {-# INLINE parseJSON #-}
--     parseJSON Null = pure Proxy
--     parseJSON v    = typeMismatch "Proxy" v


-- instance MFromJSON2 m Tagged where
--     liftParseJSON2 _ _ p _ = fmap Tagged . p
--     {-# INLINE liftParseJSON2 #-}

-- instance MFromJSON1 m (Tagged a) where
--     liftParseJSON p _ = fmap Tagged . p
--     {-# INLINE liftParseJSON #-}

-- instance MFromJSON m b => MFromJSON m (Tagged a b) where
--     parseJSON = parseJSON1
--     {-# INLINE parseJSON #-}

-- instance FromJSONKey b => FromJSONKey (Tagged a b) where
--     fromJSONKey = coerceFromJSONKeyFunction (fromJSONKey :: FromJSONKeyFunction b)
--     fromJSONKeyList = (fmap . fmap) Tagged fromJSONKeyList

-- -------------------------------------------------------------------------------
-- -- Instances for converting from map keys
-- -------------------------------------------------------------------------------

-- instance (MFromJSON m  a, MFromJSON m b) => FromJSONKey (a,b)
-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c) => FromJSONKey (a,b,c)
-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d) => FromJSONKey (a,b,c,d)

-- instance FromJSONKey Char where
--     fromJSONKey = FromJSONKeyTextParser $ \t ->
--         if T.length t == 1
--             then return (T.index t 0)
--             else typeMismatch "Expected Char but String didn't contain exactly one character" (String t)
--     fromJSONKeyList = FromJSONKeyText T.unpack

-- instance (MFromJSON m Key a, MFromJSON m a) => FromJSONKey [a] where
--     fromJSONKey = fromJSONKeyList

-- -------------------------------------------------------------------------------
-- -- Tuple instances, see tuple-instances-from.hs
-- -------------------------------------------------------------------------------

-- instance MFromJSON2 m (,) where
--     liftParseJSON2 pA _ pB _ = withArray "(a, b)" $ \t ->
--         let n = V.length t
--         in if n == 2
--             then (,)
--                 <$> parseJSONElemAtIndex pA 0 t
--                 <*> parseJSONElemAtIndex pB 1 t
--             else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 2"
--     {-# INLINE liftParseJSON2 #-}

-- instance (MFromJSON m  a) => MFromJSON1 m ((,) a) where
--     liftParseJSON = liftParseJSON2 parseJSON parseJSONList
--     {-# INLINE liftParseJSON #-}

-- instance (MFromJSON m  a, MFromJSON m b) => MFromJSON m (a, b) where
--     parseJSON = parseJSON2
--     {-# INLINE parseJSON #-}


-- instance (MFromJSON m  a) => MFromJSON2 m ((,,) a) where
--     liftParseJSON2 pB _ pC _ = withArray "(a, b, c)" $ \t ->
--         let n = V.length t
--         in if n == 3
--             then (,,)
--                 <$> parseJSONElemAtIndex parseJSON 0 t
--                 <*> parseJSONElemAtIndex pB 1 t
--                 <*> parseJSONElemAtIndex pC 2 t
--             else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 3"
--     {-# INLINE liftParseJSON2 #-}

-- instance (MFromJSON m  a, MFromJSON m b) => MFromJSON1 m ((,,) a b) where
--     liftParseJSON = liftParseJSON2 parseJSON parseJSONList
--     {-# INLINE liftParseJSON #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c) => MFromJSON m (a, b, c) where
--     parseJSON = parseJSON2
--     {-# INLINE parseJSON #-}


-- instance (MFromJSON m  a, MFromJSON m b) => MFromJSON2 m ((,,,) a b) where
--     liftParseJSON2 pC _ pD _ = withArray "(a, b, c, d)" $ \t ->
--         let n = V.length t
--         in if n == 4
--             then (,,,)
--                 <$> parseJSONElemAtIndex parseJSON 0 t
--                 <*> parseJSONElemAtIndex parseJSON 1 t
--                 <*> parseJSONElemAtIndex pC 2 t
--                 <*> parseJSONElemAtIndex pD 3 t
--             else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 4"
--     {-# INLINE liftParseJSON2 #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c) => MFromJSON1 m ((,,,) a b c) where
--     liftParseJSON = liftParseJSON2 parseJSON parseJSONList
--     {-# INLINE liftParseJSON #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d) => MFromJSON m (a, b, c, d) where
--     parseJSON = parseJSON2
--     {-# INLINE parseJSON #-}


-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c) => MFromJSON2 m ((,,,,) a b c) where
--     liftParseJSON2 pD _ pE _ = withArray "(a, b, c, d, e)" $ \t ->
--         let n = V.length t
--         in if n == 5
--             then (,,,,)
--                 <$> parseJSONElemAtIndex parseJSON 0 t
--                 <*> parseJSONElemAtIndex parseJSON 1 t
--                 <*> parseJSONElemAtIndex parseJSON 2 t
--                 <*> parseJSONElemAtIndex pD 3 t
--                 <*> parseJSONElemAtIndex pE 4 t
--             else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 5"
--     {-# INLINE liftParseJSON2 #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d) => MFromJSON1 m ((,,,,) a b c d) where
--     liftParseJSON = liftParseJSON2 parseJSON parseJSONList
--     {-# INLINE liftParseJSON #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e) => MFromJSON m (a, b, c, d, e) where
--     parseJSON = parseJSON2
--     {-# INLINE parseJSON #-}


-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d) => MFromJSON2 m ((,,,,,) a b c d) where
--     liftParseJSON2 pE _ pF _ = withArray "(a, b, c, d, e, f)" $ \t ->
--         let n = V.length t
--         in if n == 6
--             then (,,,,,)
--                 <$> parseJSONElemAtIndex parseJSON 0 t
--                 <*> parseJSONElemAtIndex parseJSON 1 t
--                 <*> parseJSONElemAtIndex parseJSON 2 t
--                 <*> parseJSONElemAtIndex parseJSON 3 t
--                 <*> parseJSONElemAtIndex pE 4 t
--                 <*> parseJSONElemAtIndex pF 5 t
--             else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 6"
--     {-# INLINE liftParseJSON2 #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e) => MFromJSON1 m ((,,,,,) a b c d e) where
--     liftParseJSON = liftParseJSON2 parseJSON parseJSONList
--     {-# INLINE liftParseJSON #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f) => MFromJSON m (a, b, c, d, e, f) where
--     parseJSON = parseJSON2
--     {-# INLINE parseJSON #-}


-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e) => MFromJSON2 m ((,,,,,,) a b c d e) where
--     liftParseJSON2 pF _ pG _ = withArray "(a, b, c, d, e, f, g)" $ \t ->
--         let n = V.length t
--         in if n == 7
--             then (,,,,,,)
--                 <$> parseJSONElemAtIndex parseJSON 0 t
--                 <*> parseJSONElemAtIndex parseJSON 1 t
--                 <*> parseJSONElemAtIndex parseJSON 2 t
--                 <*> parseJSONElemAtIndex parseJSON 3 t
--                 <*> parseJSONElemAtIndex parseJSON 4 t
--                 <*> parseJSONElemAtIndex pF 5 t
--                 <*> parseJSONElemAtIndex pG 6 t
--             else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 7"
--     {-# INLINE liftParseJSON2 #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f) => MFromJSON1 m ((,,,,,,) a b c d e f) where
--     liftParseJSON = liftParseJSON2 parseJSON parseJSONList
--     {-# INLINE liftParseJSON #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f, MFromJSON m g) => MFromJSON m (a, b, c, d, e, f, g) where
--     parseJSON = parseJSON2
--     {-# INLINE parseJSON #-}


-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f) => MFromJSON2 m ((,,,,,,,) a b c d e f) where
--     liftParseJSON2 pG _ pH _ = withArray "(a, b, c, d, e, f, g, h)" $ \t ->
--         let n = V.length t
--         in if n == 8
--             then (,,,,,,,)
--                 <$> parseJSONElemAtIndex parseJSON 0 t
--                 <*> parseJSONElemAtIndex parseJSON 1 t
--                 <*> parseJSONElemAtIndex parseJSON 2 t
--                 <*> parseJSONElemAtIndex parseJSON 3 t
--                 <*> parseJSONElemAtIndex parseJSON 4 t
--                 <*> parseJSONElemAtIndex parseJSON 5 t
--                 <*> parseJSONElemAtIndex pG 6 t
--                 <*> parseJSONElemAtIndex pH 7 t
--             else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 8"
--     {-# INLINE liftParseJSON2 #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f, MFromJSON m g) => MFromJSON1 m ((,,,,,,,) a b c d e f g) where
--     liftParseJSON = liftParseJSON2 parseJSON parseJSONList
--     {-# INLINE liftParseJSON #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f, MFromJSON m g, MFromJSON m h) => MFromJSON m (a, b, c, d, e, f, g, h) where
--     parseJSON = parseJSON2
--     {-# INLINE parseJSON #-}


-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f, MFromJSON m g) => MFromJSON2 m ((,,,,,,,,) a b c d e f g) where
--     liftParseJSON2 pH _ pI _ = withArray "(a, b, c, d, e, f, g, h, i)" $ \t ->
--         let n = V.length t
--         in if n == 9
--             then (,,,,,,,,)
--                 <$> parseJSONElemAtIndex parseJSON 0 t
--                 <*> parseJSONElemAtIndex parseJSON 1 t
--                 <*> parseJSONElemAtIndex parseJSON 2 t
--                 <*> parseJSONElemAtIndex parseJSON 3 t
--                 <*> parseJSONElemAtIndex parseJSON 4 t
--                 <*> parseJSONElemAtIndex parseJSON 5 t
--                 <*> parseJSONElemAtIndex parseJSON 6 t
--                 <*> parseJSONElemAtIndex pH 7 t
--                 <*> parseJSONElemAtIndex pI 8 t
--             else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 9"
--     {-# INLINE liftParseJSON2 #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f, MFromJSON m g, MFromJSON m h) => MFromJSON1 m ((,,,,,,,,) a b c d e f g h) where
--     liftParseJSON = liftParseJSON2 parseJSON parseJSONList
--     {-# INLINE liftParseJSON #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f, MFromJSON m g, MFromJSON m h, MFromJSON m i) => MFromJSON m (a, b, c, d, e, f, g, h, i) where
--     parseJSON = parseJSON2
--     {-# INLINE parseJSON #-}


-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f, MFromJSON m g, MFromJSON m h) => MFromJSON2 m ((,,,,,,,,,) a b c d e f g h) where
--     liftParseJSON2 pI _ pJ _ = withArray "(a, b, c, d, e, f, g, h, i, j)" $ \t ->
--         let n = V.length t
--         in if n == 10
--             then (,,,,,,,,,)
--                 <$> parseJSONElemAtIndex parseJSON 0 t
--                 <*> parseJSONElemAtIndex parseJSON 1 t
--                 <*> parseJSONElemAtIndex parseJSON 2 t
--                 <*> parseJSONElemAtIndex parseJSON 3 t
--                 <*> parseJSONElemAtIndex parseJSON 4 t
--                 <*> parseJSONElemAtIndex parseJSON 5 t
--                 <*> parseJSONElemAtIndex parseJSON 6 t
--                 <*> parseJSONElemAtIndex parseJSON 7 t
--                 <*> parseJSONElemAtIndex pI 8 t
--                 <*> parseJSONElemAtIndex pJ 9 t
--             else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 10"
--     {-# INLINE liftParseJSON2 #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f, MFromJSON m g, MFromJSON m h, MFromJSON m i) => MFromJSON1 m ((,,,,,,,,,) a b c d e f g h i) where
--     liftParseJSON = liftParseJSON2 parseJSON parseJSONList
--     {-# INLINE liftParseJSON #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f, MFromJSON m g, MFromJSON m h, MFromJSON m i, MFromJSON m j) => MFromJSON m (a, b, c, d, e, f, g, h, i, j) where
--     parseJSON = parseJSON2
--     {-# INLINE parseJSON #-}


-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f, MFromJSON m g, MFromJSON m h, MFromJSON m i) => MFromJSON2 m ((,,,,,,,,,,) a b c d e f g h i) where
--     liftParseJSON2 pJ _ pK _ = withArray "(a, b, c, d, e, f, g, h, i, j, k)" $ \t ->
--         let n = V.length t
--         in if n == 11
--             then (,,,,,,,,,,)
--                 <$> parseJSONElemAtIndex parseJSON 0 t
--                 <*> parseJSONElemAtIndex parseJSON 1 t
--                 <*> parseJSONElemAtIndex parseJSON 2 t
--                 <*> parseJSONElemAtIndex parseJSON 3 t
--                 <*> parseJSONElemAtIndex parseJSON 4 t
--                 <*> parseJSONElemAtIndex parseJSON 5 t
--                 <*> parseJSONElemAtIndex parseJSON 6 t
--                 <*> parseJSONElemAtIndex parseJSON 7 t
--                 <*> parseJSONElemAtIndex parseJSON 8 t
--                 <*> parseJSONElemAtIndex pJ 9 t
--                 <*> parseJSONElemAtIndex pK 10 t
--             else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 11"
--     {-# INLINE liftParseJSON2 #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f, MFromJSON m g, MFromJSON m h, MFromJSON m i, MFromJSON m j) => MFromJSON1 m ((,,,,,,,,,,) a b c d e f g h i j) where
--     liftParseJSON = liftParseJSON2 parseJSON parseJSONList
--     {-# INLINE liftParseJSON #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f, MFromJSON m g, MFromJSON m h, MFromJSON m i, MFromJSON m j, MFromJSON m k) => MFromJSON m (a, b, c, d, e, f, g, h, i, j, k) where
--     parseJSON = parseJSON2
--     {-# INLINE parseJSON #-}


-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f, MFromJSON m g, MFromJSON m h, MFromJSON m i, MFromJSON m j) => MFromJSON2 m ((,,,,,,,,,,,) a b c d e f g h i j) where
--     liftParseJSON2 pK _ pL _ = withArray "(a, b, c, d, e, f, g, h, i, j, k, l)" $ \t ->
--         let n = V.length t
--         in if n == 12
--             then (,,,,,,,,,,,)
--                 <$> parseJSONElemAtIndex parseJSON 0 t
--                 <*> parseJSONElemAtIndex parseJSON 1 t
--                 <*> parseJSONElemAtIndex parseJSON 2 t
--                 <*> parseJSONElemAtIndex parseJSON 3 t
--                 <*> parseJSONElemAtIndex parseJSON 4 t
--                 <*> parseJSONElemAtIndex parseJSON 5 t
--                 <*> parseJSONElemAtIndex parseJSON 6 t
--                 <*> parseJSONElemAtIndex parseJSON 7 t
--                 <*> parseJSONElemAtIndex parseJSON 8 t
--                 <*> parseJSONElemAtIndex parseJSON 9 t
--                 <*> parseJSONElemAtIndex pK 10 t
--                 <*> parseJSONElemAtIndex pL 11 t
--             else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 12"
--     {-# INLINE liftParseJSON2 #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f, MFromJSON m g, MFromJSON m h, MFromJSON m i, MFromJSON m j, MFromJSON m k) => MFromJSON1 m ((,,,,,,,,,,,) a b c d e f g h i j k) where
--     liftParseJSON = liftParseJSON2 parseJSON parseJSONList
--     {-# INLINE liftParseJSON #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f, MFromJSON m g, MFromJSON m h, MFromJSON m i, MFromJSON m j, MFromJSON m k, MFromJSON m l) => MFromJSON m (a, b, c, d, e, f, g, h, i, j, k, l) where
--     parseJSON = parseJSON2
--     {-# INLINE parseJSON #-}


-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f, MFromJSON m g, MFromJSON m h, MFromJSON m i, MFromJSON m j, MFromJSON m k) => MFromJSON2 m ((,,,,,,,,,,,,) a b c d e f g h i j k) where
--     liftParseJSON2 pL _ pM _ = withArray "(a, b, c, d, e, f, g, h, i, j, k, l, m)" $ \t ->
--         let n = V.length t
--         in if n == 13
--             then (,,,,,,,,,,,,)
--                 <$> parseJSONElemAtIndex parseJSON 0 t
--                 <*> parseJSONElemAtIndex parseJSON 1 t
--                 <*> parseJSONElemAtIndex parseJSON 2 t
--                 <*> parseJSONElemAtIndex parseJSON 3 t
--                 <*> parseJSONElemAtIndex parseJSON 4 t
--                 <*> parseJSONElemAtIndex parseJSON 5 t
--                 <*> parseJSONElemAtIndex parseJSON 6 t
--                 <*> parseJSONElemAtIndex parseJSON 7 t
--                 <*> parseJSONElemAtIndex parseJSON 8 t
--                 <*> parseJSONElemAtIndex parseJSON 9 t
--                 <*> parseJSONElemAtIndex parseJSON 10 t
--                 <*> parseJSONElemAtIndex pL 11 t
--                 <*> parseJSONElemAtIndex pM 12 t
--             else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 13"
--     {-# INLINE liftParseJSON2 #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f, MFromJSON m g, MFromJSON m h, MFromJSON m i, MFromJSON m j, MFromJSON m k, MFromJSON m l) => MFromJSON1 m ((,,,,,,,,,,,,) a b c d e f g h i j k l) where
--     liftParseJSON = liftParseJSON2 parseJSON parseJSONList
--     {-# INLINE liftParseJSON #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f, MFromJSON m g, MFromJSON m h, MFromJSON m i, MFromJSON m j, MFromJSON m k, MFromJSON m l, MFromJSON m m) => MFromJSON m (a, b, c, d, e, f, g, h, i, j, k, l, m) where
--     parseJSON = parseJSON2
--     {-# INLINE parseJSON #-}


-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f, MFromJSON m g, MFromJSON m h, MFromJSON m i, MFromJSON m j, MFromJSON m k, MFromJSON m l) => MFromJSON2 m ((,,,,,,,,,,,,,) a b c d e f g h i j k l) where
--     liftParseJSON2 pM _ pN _ = withArray "(a, b, c, d, e, f, g, h, i, j, k, l, m, n)" $ \t ->
--         let n = V.length t
--         in if n == 14
--             then (,,,,,,,,,,,,,)
--                 <$> parseJSONElemAtIndex parseJSON 0 t
--                 <*> parseJSONElemAtIndex parseJSON 1 t
--                 <*> parseJSONElemAtIndex parseJSON 2 t
--                 <*> parseJSONElemAtIndex parseJSON 3 t
--                 <*> parseJSONElemAtIndex parseJSON 4 t
--                 <*> parseJSONElemAtIndex parseJSON 5 t
--                 <*> parseJSONElemAtIndex parseJSON 6 t
--                 <*> parseJSONElemAtIndex parseJSON 7 t
--                 <*> parseJSONElemAtIndex parseJSON 8 t
--                 <*> parseJSONElemAtIndex parseJSON 9 t
--                 <*> parseJSONElemAtIndex parseJSON 10 t
--                 <*> parseJSONElemAtIndex parseJSON 11 t
--                 <*> parseJSONElemAtIndex pM 12 t
--                 <*> parseJSONElemAtIndex pN 13 t
--             else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 14"
--     {-# INLINE liftParseJSON2 #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f, MFromJSON m g, MFromJSON m h, MFromJSON m i, MFromJSON m j, MFromJSON m k, MFromJSON m l, MFromJSON m m) => MFromJSON1 m ((,,,,,,,,,,,,,) a b c d e f g h i j k l m) where
--     liftParseJSON = liftParseJSON2 parseJSON parseJSONList
--     {-# INLINE liftParseJSON #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f, MFromJSON m g, MFromJSON m h, MFromJSON m i, MFromJSON m j, MFromJSON m k, MFromJSON m l, MFromJSON m m, MFromJSON m n) => MFromJSON m (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
--     parseJSON = parseJSON2
--     {-# INLINE parseJSON #-}


-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f, MFromJSON m g, MFromJSON m h, MFromJSON m i, MFromJSON m j, MFromJSON m k, MFromJSON m l, MFromJSON m m) => MFromJSON2 m ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m) where
--     liftParseJSON2 pN _ pO _ = withArray "(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)" $ \t ->
--         let n = V.length t
--         in if n == 15
--             then (,,,,,,,,,,,,,,)
--                 <$> parseJSONElemAtIndex parseJSON 0 t
--                 <*> parseJSONElemAtIndex parseJSON 1 t
--                 <*> parseJSONElemAtIndex parseJSON 2 t
--                 <*> parseJSONElemAtIndex parseJSON 3 t
--                 <*> parseJSONElemAtIndex parseJSON 4 t
--                 <*> parseJSONElemAtIndex parseJSON 5 t
--                 <*> parseJSONElemAtIndex parseJSON 6 t
--                 <*> parseJSONElemAtIndex parseJSON 7 t
--                 <*> parseJSONElemAtIndex parseJSON 8 t
--                 <*> parseJSONElemAtIndex parseJSON 9 t
--                 <*> parseJSONElemAtIndex parseJSON 10 t
--                 <*> parseJSONElemAtIndex parseJSON 11 t
--                 <*> parseJSONElemAtIndex parseJSON 12 t
--                 <*> parseJSONElemAtIndex pN 13 t
--                 <*> parseJSONElemAtIndex pO 14 t
--             else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 15"
--     {-# INLINE liftParseJSON2 #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f, MFromJSON m g, MFromJSON m h, MFromJSON m i, MFromJSON m j, MFromJSON m k, MFromJSON m l, MFromJSON m m, MFromJSON m n) => MFromJSON1 m ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m n) where
--     liftParseJSON = liftParseJSON2 parseJSON parseJSONList
--     {-# INLINE liftParseJSON #-}

-- instance (MFromJSON m  a, MFromJSON m b, MFromJSON m c, MFromJSON m d, MFromJSON m e, MFromJSON m f, MFromJSON m g, MFromJSON m h, MFromJSON m i, MFromJSON m j, MFromJSON m k, MFromJSON m l, MFromJSON m m, MFromJSON m n, MFromJSON m o) => MFromJSON m (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
--     parseJSON = parseJSON2
--     {-# INLINE parseJSON #-}
