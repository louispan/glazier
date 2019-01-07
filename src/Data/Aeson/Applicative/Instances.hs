{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
{-# LANGUAGE ViewPatterns #-}

module Data.Aeson.Applicative.Instances where

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
import Data.Maybe
import Control.Applicative (Const(..), (<|>))
import Control.Monad (zipWithM, (<=<))
import Data.Aeson.Applicative.Internal
import qualified Data.Aeson.Encoding.Internal as E
-- import qualified Data.Aeson.Compat as Compat
import Data.Aeson.Internal
-- import Data.Aeson.Internal.Functions (mapKey)
-- import Data.Aeson.Parser.Internal (eitherDecodeWith, jsonEOF)
-- import qualified Data.Aeson.Parser.Time as Time
import Data.Aeson.Types
-- import Data.Aeson.Types.Generic
-- import Data.Aeson.Types.Internal
import qualified Data.Attoparsec.ByteString.Char8 as A
    ( endOfInput
    , parseOnly
    , scientific
    )
import qualified Data.Attoparsec.ByteString.Char8 as A
    ( endOfInput
    , parseOnly
    , scientific
    )
import Data.Bits (unsafeShiftR)
import Data.DList (DList)
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
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as M
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Monoid as Monoid
import qualified Data.Primitive.Types as PM
import Data.Proxy (Proxy(..))
import Data.Ratio (Ratio, (%))
import Data.Scientific (Scientific, base10Exponent)
import qualified Data.Scientific as Scientific
import Data.Semigroup ((<>))
import qualified Data.Semigroup as Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Tagged (Tagged(..))
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
import Data.Time.Format (parseTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Data.Traversable as Tr (sequence)
import qualified Data.Tree as Tree
import qualified Data.UUID.Types as UUID
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Data.Version (Version, parseVersion)
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types (CTime(..))
import Foreign.Storable (Storable)
import GHC.Generics
import Numeric.Natural (Natural)
import Prelude.Compat
import Text.ParserCombinators.ReadP (readP_to_S)
import Unsafe.Coerce (unsafeCoerce)
import qualified GHC.Exts as Exts

#if MIN_VERSION_base(4,7,0)
import qualified Data.Primitive.Array as PM
#if MIN_VERSION_primitive(0,6,4)
import qualified Data.Primitive.SmallArray as PM
import qualified Data.Primitive.PrimArray as PM
import qualified Data.Primitive.UnliftedArray as PM
#endif
#endif

-- Based on aeson-1.4.2.0

-- #############################################################################
-- Data.Aeson.Types.ToJSON
-- #############################################################################

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- instance Applicative m => MToJSON2 m Const where
--     mmliftToEncoding2 t _ _ _ (Const x) = t x
--     {-# INLINE mmliftToEncoding2 #-}

-- instance Applicative m => MToJSON m a => MToJSON1 m (Const a) where
--     mmliftToEncoding _ _ (Const x) = mmtoEncoding x
--     {-# INLINE mmliftToEncoding #-}

-- instance Applicative m => MToJSON m a => MToJSON m (Const a b) where
--     mmtoEncoding (Const x) = mmtoEncoding x
--     {-# INLINE mmtoEncoding #-}

-- instance Applicative m => MFromJSON2 m Const where
--     mliftParseJSON2 p _ _ _ = fmap (fmap Const) . p
--     {-# INLINE mliftParseJSON2 #-}

-- instance MFromJSON m a => MFromJSON1 m (Const a) where
--     mliftParseJSON _ _ = fmap (fmap Const) . mparseJSON
--     {-# INLINE mliftParseJSON #-}

-- instance MFromJSON m a => MFromJSON m (Const a b) where
--     mparseJSON = fmap (fmap Const) . mparseJSON
--     {-# INLINE mparseJSON #-}



-- #############################################################################
-- Data.Aeson.Types.FromJSON
-- #############################################################################
