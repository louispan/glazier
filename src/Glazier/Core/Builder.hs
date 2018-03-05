-- {-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Glazier.Core.Builder where

-- import Control.Applicative
import Control.Lens
import Control.Monad.Trans.Reader
import Data.Biapplicative
import Data.Diverse.Lens

------------------------------------------------

-- -- | Make inactive model, monadic as it may need to create IORefs
-- newtype MkSpec r m s = MkSpec {
--             unMkSpec :: r -> m s
--             } deriving (Functor)

-- instance Applicative m => Applicative (MkSpec r m) where
--     pure a = MkSpec $ const $ pure a
--     (<*>) = MkSpec

------------------------------------------------

-- newtype MkSpecOnInfo m s r = MkSpecOnInfo { unMkSpecOnInfo :: MkSpec m r s }

-- instance Z.ViaInfo (MkSpecOnInfo m s) where
--     type OnInfo (MkSpecOnInfo m s) r = MkSpec m r s
--     viaInfo l (MkSpec mkSpc) = MkSpec $ mkSpc . view l

-- instance Contravariant (MkSpecOnInfo m s) where
--     contramap f (MkSpecOnInfo (MkSpec g)) = coerce (g . f)

------------------------------------------------

-- -- | Monadic because we may need to 'Z.doReadIORef' to get the data to make the info.
-- newtype MkReq s m r = MkReq {
--             unMkReq :: s -> m r
--             } deriving (Functor)

------------------------------------------------

-- newtype MkReqOnSpec m r s = MkReqOnSpec { unMkReqOnSpec :: MkReq m s r }

-- instance Z.ViaSpec (MkReqOnSpec m i) where
--     type OnSpec (MkReqOnSpec m i) s = MkReq m s i
--     viaSpec l (MkReq mkReq) = MkReq $ mkReq . view l

-- instance Contravariant (MkReqOnSpec m s) where
--     contramap f (MkReqOnSpec (MkReq g)) = coerce (g . f)

------------------------------------------------

-- | @p' s'@ are the types the builder knows how to make
-- @p s@ are the type the builder knows how to read
data Builder r s m r' s' = Builder
    { mkReq :: ReaderT s m r' -- from specifications
    , mkSpec :: ReaderT r m s' -- make inactive specifications
    }

instance Functor m => Bifunctor (Builder r s m) where
    bimap ij st (Builder mkRq mkSpc) = Builder (ij <$> mkRq) (st <$> mkSpc)

instance Applicative m => Biapplicative (Builder r s m) where
    bipure r s = Builder (pure r) (pure s)
    (Builder fMkRq fMkMdl) <<*>> (Builder mkRq mkSpc) =
        Builder (fMkRq <*> mkRq) (fMkMdl <*> mkSpc)

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

-- -- | THe identity for 'andBuilder'
-- nulBuilder :: Applicative m => Builder m r s (Many '[]) (Many '[])
-- nulBuilder = bipure nil nil

-- -- | A friendlier constraint synonym for 'also2' for 'Builder'.
-- type AlsoBuilder r1 r2 r3 s1 s2 s3 =
--     ( r3 ~ Append r1 r2
--     , s3 ~ Append s1 s2
--     )

-- andBuilder ::
--     (Applicative m
--     , AndBuilder r1 r2 r3 s1 s2 s3
--     ) =>
--     Builder m r s (Many r1) (Many s1)
--     -> Builder m r s (Many r2) (Many s2)
--     -> Builder m r s (Many r3) (Many s3)
-- infixr 6 `andBuilder` -- like mappend
-- (Builder (MkReq mkReq, MkSpec mkSpc)) `andBuilder`
--     (Builder (MkReq mkReq', MkSpec mkSpc')) =
--         Builder
--             ( MkReq $ \s -> (/./) <$> mkReq s <*> mkReq' s
--             , MkSpec $ \r -> (/./) <$> mkSpc r <*> mkSpc' r)

-- instance (Applicative m, AlsoBuilder r1 r2 r3 s1 s2 s3) => Z.Also2 (Builder r s m)
--     (Many r1) (Many r2) (Many r3)
--     (Many s1) (Many s2) (Many s3) where
--     (Builder mkReq mkSpc) `also2` (Builder mkReq' mkSpc') = Builder
--         (liftA2 (/./) mkReq mkReq')
--         (liftA2 (/./) mkSpc mkSpc')

-- -- | A type restricted verison of const
-- -- where the right builder is a 'nulBuilder'.
-- -- It is useful for double checking that we can throw away the 'nulBuilder'
-- constBuilder :: Builder m r s r' s' -> Builder m r s (Many '[]) (Many '[]) -> Builder m r s r' s'
-- constBuilder = const

------------------------------------------------

-- | Modify Builder's reading environment @i1@ and @s1@ inside a larger @i2@ @s2@
enlargeB ::
    (r2 -> r1)
    -> (s2 -> s1)
    -> Builder r1 s1 m r' s'
    -> Builder r2 s2 m r' s'
enlargeB fr fs (Builder mkRq mkSpc) = Builder
    (withReaderT fs mkRq)
    (withReaderT fr mkSpc)

-- | Modify Builder's reading environment @i1@ and @s1@ inside a larger @i2@ @s2@
magnifyB :: Monad m =>
    (Lens' r2 r1)
    -> (Lens' s2 s1)
    -> Builder r1 s1 m r' s'
    -> Builder r2 s2 m r' s'
magnifyB rl sl (Builder mkRq mkSpc) = Builder
    (magnify sl mkRq)
    (magnify rl mkSpc)

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
build :: forall x m. (Monad m)
    => Builder x x m x x
build = Builder ask ask

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
hardcode :: Applicative m => x -> Builder r s m (Many '[]) x
hardcode x = Builder (pure nil) (pure x)

-- | Add a value @x@ into the model that is not from the info.
hardcodeItem :: Applicative m => x -> Builder r s m (Many '[]) (Many '[x])
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

