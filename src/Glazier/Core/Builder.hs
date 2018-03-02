{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.Core.Builder where

import Control.Lens
import Data.Biapplicative
import Data.Diverse.Lens

------------------------------------------------

-- | Make inactive model, monadic as it may need to create IORefs
newtype MkSpec m r s = MkSpec {
            unMkSpec :: r -> m s
            } deriving Functor

------------------------------------------------

-- newtype MkSpecOnInfo m s r = MkSpecOnInfo { unMkSpecOnInfo :: MkSpec m r s }

-- instance Z.ViaInfo (MkSpecOnInfo m s) where
--     type OnInfo (MkSpecOnInfo m s) r = MkSpec m r s
--     viaInfo l (MkSpec mkSpc) = MkSpec $ mkSpc . view l

-- instance Contravariant (MkSpecOnInfo m s) where
--     contramap f (MkSpecOnInfo (MkSpec g)) = coerce (g . f)

------------------------------------------------

-- | Monadic because we may need to 'Z.doReadIORef' to get the data to make the info.
newtype MkReq m s r = MkReq {
            unMkReq :: s -> m r
            } deriving Functor

------------------------------------------------

newtype MkReqOnSpec m r s = MkReqOnSpec { unMkReqOnSpec :: MkReq m s r }

-- instance Z.ViaSpec (MkReqOnSpec m i) where
--     type OnSpec (MkReqOnSpec m i) s = MkReq m s i
--     viaSpec l (MkReq mkReq) = MkReq $ mkReq . view l

-- instance Contravariant (MkReqOnSpec m s) where
--     contramap f (MkReqOnSpec (MkReq g)) = coerce (g . f)

------------------------------------------------

-- | @p' s'@ are the types the builder knows how to make
-- @p s@ are the type the builder knows how to read
newtype Builder m r s r' s' =
    Builder ( MkReq m s r' -- from specifications
            , MkSpec m r s' -- make inactive specifications
            )

instance Functor m => Bifunctor (Builder m r s) where
    bimap ij st (Builder (mkReq, mkSpc)) = Builder (ij <$> mkReq, st <$> mkSpc)

instance Applicative m => Biapplicative (Builder m r s) where
    bipure r s = Builder (MkReq . const $ pure r, MkSpec . const $ pure s)
    (Builder (MkReq fMkReq, MkSpec fMkMdl)) <<*>> (Builder (MkReq mkReq, MkSpec mkSpc)) =
        Builder ( MkReq $ \r -> fMkReq r <*> mkReq r
                , MkSpec $ \s -> fMkMdl s <*> mkSpc s
                )

-- ------------------------------------------------

-- newtype BuilderOnInfo m s r' s' r = BuilderOnInfo { unBuilderOnInfo :: Builder m r s r' s' }

-- instance Z.ViaInfo (BuilderOnInfo m s r' s') where
--     type OnInfo (BuilderOnInfo m s r' s') r = Builder m r s r' s'
--     viaInfo l (Builder (mkReq, mkSpc)) =
--         Builder (mkReq, Z.viaInfo l mkSpc)

-- ------------------------------------------------

-- newtype BuilderOnSpec m r r' s' s = BuilderOnSpec { unBuilderOnSpec :: Builder m r s r' s' }

-- instance Z.ViaSpec (BuilderOnSpec m r r' s') where
--     type OnSpec (BuilderOnSpec m r r' s') s = Builder m r s r' s'
--     viaSpec l (Builder (mkReq, mkSpc)) =
--         Builder (Z.viaSpec l mkReq, mkSpc)

-- ------------------------------------------------

-- | THe identity for 'andBuilder'
nulBuilder :: Applicative m => Builder m r s (Many '[]) (Many '[])
nulBuilder = bipure nil nil

-- | A friendlier constraint synonym for 'PBuilder' 'pmappend'.
type AndBuilder r1 r2 r3 s1 s2 s3 =
    ( r3 ~ Append r1 r2
    , s3 ~ Append s1 s2
    )

andBuilder ::
    (Applicative m
    , AndBuilder r1 r2 r3 s1 s2 s3
    ) =>
    Builder m r s (Many r1) (Many s1)
    -> Builder m r s (Many r2) (Many s2)
    -> Builder m r s (Many r3) (Many s3)
infixr 6 `andBuilder` -- like mappend
(Builder (MkReq mkReq, MkSpec mkSpc)) `andBuilder`
    (Builder (MkReq mkReq', MkSpec mkSpc')) =
        Builder
            ( MkReq $ \s -> (/./) <$> mkReq s <*> mkReq' s
            , MkSpec $ \r -> (/./) <$> mkSpc r <*> mkSpc' r)

-- -- | A type restricted verison of const
-- -- where the right builder is a 'nulBuilder'.
-- -- It is useful for double checking that we can throw away the 'nulBuilder'
-- constBuilder :: Builder m r s r' s' -> Builder m r s (Many '[]) (Many '[]) -> Builder m r s r' s'
-- constBuilder = const

------------------------------------------------

-- | Modify Builder's reading environment @i1@ and @s1@ inside a larger @i2@ @s2@
enlargeBuilder ::
    (r2 -> r1)
    -> (s2 -> s1)
    -> Builder m r1 s1 r' s' -> Builder m r2 s2 r' s'
enlargeBuilder fr fs (Builder (MkReq mkReq, MkSpec mkSpc)) =
    Builder
        ( MkReq (mkReq . fs)
        , MkSpec (mkSpc . fr))

-- -- | Return a builder that builds an item inside a Many
-- toItemBuilder
--     :: forall m r1 r2 s1 s2 r' s'.
--     ( Applicative m
--     )
--     => (r2 -> r1)
--     -> (s2 -> s1)
--     -> Builder m r1 s1 r' s'
--     -> Builder m r2 s2 (Many '[r']) (Many '[s'])
-- toItemBuilder fr fs bld = enlargeBuilder fr fs
--     $ bimap single single bld

-- | Add a type @x@ into the model that is used directly from the info.
-- @forall@ so that the type can be specified first
build :: forall x m. (Applicative m)
    => Builder m x x x x
build = Builder ( MkReq pure
                , MkSpec pure
                )

-- -- | Add a type @x@ into the model that is used directly from the info
-- -- and return a builder that uses a Many.
-- -- @forall@ so that the type can be specified first
-- buildItem :: forall x m r s. (Applicative m)
--     => (r -> x) -> (s -> x) -> Builder m r s (Many '[x]) (Many '[x])
-- buildItem fr fs = toItemBuilder fr fs build

-- -- | Add a type @x@ into the model that is used directly from the info
-- -- and return a builder that uses a Many.
-- -- @forall@ so that the type can be specified first
-- buildItem
--     :: forall x m r s. (Applicative m)s
--     => Builder m r s (Many '[x]) (Many '[x])
-- buildItem = bimap single

-- | Add a value @x@ into the model that is not from the info.
hardcode :: Applicative m => x -> Builder m r s (Many '[]) x
hardcode x = Builder
    ( MkReq . const $ pure nil
    , MkSpec . const $ pure x
    )

-- | Add a value @x@ into the model that is not from the info.
hardcodeItem :: Applicative m => x -> Builder m r s (Many '[]) (Many '[x])
hardcodeItem = bimap id single . hardcode

-- -- | Add a value @x@ into the model that is not from the info.
-- -- @forall@ so that the type can be specified first
-- hardcodeItem
--     :: forall x m r s. Applicative m
--     => x -> Builder m r s (Many '[]) (Many '[x])
-- hardcodeItem x = Builder ( MkReq . const $ pure nil
--                   , MkSpec . const . pure $ single x
--                   )

-- -- | Add a value @x@ into the model that is not from the info.
-- -- @forall@ so that the type can be specified first
-- -- @AllowAmbiguousTypes@: Use @TypeApplications@ instead of @Proxy@ to specify @t@
-- hardcodeItemTag
--     :: forall t x m r s. Applicative m
--     => x -> Builder m r s (Many '[]) (Many '[Tagged t x])
-- hardcodeItemTag x = Builder ( MkReq . const $ pure nil
--                   , MkSpec . const . pure . single $ Tagged x
--                   )

-- -- | More descriptive name for 'second' for Builder
-- mapModel :: Functor m => (s' -> t') -> Builder m r s r' s' -> Builder m r s r' t'
-- mapModel = second

-- -- | More descriptive name for 'first' for Builder
-- mapInfo :: Functor m => (r' -> j') -> Builder m r s r' s' -> Builder m r s j' s'
-- mapInfo = first

-- dimapInfo :: Functor m => (j -> i) -> (r' -> j') -> Builder m r s r' s' -> Builder m j s j' s'
-- dimapInfo ji ij (Builder (mkReq, mkSpc)) =
--     Builder ( ij <$> mkReq
--             , coerce (contramap ji (MkSpecOnInfo mkSpc)))

-- dimapModel :: Functor m => (t -> s) -> (s' -> t') -> Builder m r s r' s' -> Builder m r t r' t'
-- dimapModel ts st (Builder (mkReq, mkSpc)) =
--     Builder ( coerce (contramap ts (MkReqOnSpec mkReq))
--             , st <$> mkSpc)

-- transformBuilder :: Functor m
--     => (j -> i) -> (r' -> j')
--     -> (t -> s) -> (s' -> t')
--     -> Builder m r s r' s' -> Builder m j t j' t'
-- transformBuilder ji ij ts st (Builder (mkReq, mkSpc)) =
--     Builder
--         ( coerce (contramap ts (MkReqOnSpec (ij <$> mkReq)))
--         , coerce (contramap ji (MkSpecOnInfo (st <$> mkSpc))))


-- bimapBuilder :: Functor m
--     => (r' -> j')
--     -> (s' -> t')
--     -> Builder m r s r' s' -> Builder m r s j' t'
-- bimapBuilder ij st (Builder (mkReq, mkSpc)) =
--     Builder
--         ( coerce (MkReqOnSpec (ij <$> mkReq))
--         , coerce (MkSpecOnInfo (st <$> mkSpc)))

-- taggedBuilder :: forall t m r s r' s'.
--     Functor m
--     => Builder m r s r' s' -> Builder m (Tagged t i) (Tagged t s) (Tagged t r') (Tagged t s')
-- taggedBuilder = dimapModel unTagged Tagged . dimapInfo unTagged Tagged

