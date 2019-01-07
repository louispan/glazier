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

module Data.Aeson.Applicative.Internal.Generic where

import Data.Maybe
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
import Data.Bits (unsafeShiftR)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Functor.Compose
import qualified Data.HashMap.Strict as H
import Data.List (intersperse)
import qualified Data.Map.Strict as M
import Data.Scientific
import Data.Semigroup as Semigroup
import Data.Tagged
import Data.Text (Text, pack, unpack)
import qualified Data.Vector as V
import GHC.Generics
import Unsafe.Coerce

-- Based on aeson-1.4.2.0

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

newtype Tagged2 (s :: * -> *) b = Tagged2 {unTagged2 :: b}

--------------------------------------------------------------------------------

class And bool1 bool2 bool3 | bool1 bool2 -> bool3 where

instance And 'True  'True  'True
instance And 'False 'False 'False
instance And 'False 'True  'False
instance And 'True  'False 'False

--------------------------------------------------------------------------------

class ProductSize f where
    productSize :: Tagged2 f Int

instance (ProductSize a, ProductSize b) => ProductSize (a :*: b) where
    productSize = Tagged2 $ unTagged2 (productSize :: Tagged2 a Int) +
                            unTagged2 (productSize :: Tagged2 b Int)

instance ProductSize (S1 s a) where
    productSize = Tagged2 1










