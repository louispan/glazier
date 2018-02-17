-- {-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Core.Topic where

import Control.Applicative
import Control.Arrow
import qualified Control.Category as C
import Control.Lens
import Data.Profunctor
import Data.Semigroup
import qualified Glazier.React.Framework.Core.Model as F
import qualified Glazier.React.Framework.Core.Obj as F

------------------------------------------------------

-- Reader wrapper around a Profunctor
newtype Topic r p a b = Topic
    { runTopic :: r -> p a b
    } deriving (Functor, Semigroup, Monoid)

-- | Connect in parallel. Input is feed to both gates and the output of
-- both gates are '<>' together.
instance Applicative (p a) => Applicative (Topic r p a) where
    pure b = Topic $ pure (pure b)
    -- (ReaderArrow f) <*> (ReaderArrow a) = ReaderArrow $ \r -> (f r <*> a r)
    (Topic f) <*> (Topic a) = Topic $ liftA2 (<*>) f a

instance Monad (p x) => Monad (Topic r p x) where
    (Topic a) >>= f = Topic $ \r -> a r >>= (($ r) . runTopic . f)

instance Profunctor p => Profunctor (Topic r p) where
    dimap f g (Topic p) = Topic $ \r -> dimap f g (p r)

instance Strong p => Strong (Topic r p) where
    first' (Topic p) = Topic $ \r -> first' (p r)
    second' (Topic p) = Topic $ \r -> second' (p r)

instance Choice p => Choice (Topic r p) where
    left' (Topic p) = Topic $ \r -> left' (p r)
    right' (Topic p) = Topic $ \r -> right' (p r)

-- | A Topic is a category, which means you can use '>>>' to connect 'Gates' serially.
instance C.Category p => C.Category (Topic r p) where
    id = Topic $ const C.id
    (Topic bc) . (Topic ab) = Topic $ \r -> (bc r) C.. (ab r)

-- | Additional annotation on inputs are just copied to the output.
instance Arrow p => Arrow (Topic r p) where
    arr f = Topic . const $ arr f
    first (Topic p) = Topic $ \r -> first (p r)
    second (Topic p) = Topic $ \r -> second (p r)

instance ArrowChoice p => ArrowChoice (Topic r p) where
    left (Topic p) = Topic $ \r -> left (p r)
    right (Topic p) = Topic $ \r -> right (p r)

newtype TopicOnSpec p a b r = TopicOnSpec { runTopicOnSpec :: Topic r p a b }

instance F.ViaSpec (TopicOnSpec p a b) where
    type OnSpec (TopicOnSpec p a b) s = Topic s p a b
    viaSpec l (Topic p) = Topic $ p . view l

newtype TopicOnObj p a b v s = TopicOnObj { runTopicOnObj :: Topic (F.Obj v s) p a b }

instance F.ViaObj (TopicOnObj p a b v) where
    type OnObj (TopicOnObj p a b v) s = Topic (F.Obj v s) p a b
    viaObj l (Topic p) = Topic $ \obj -> p (F.edit l obj)
