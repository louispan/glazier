{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.Class where

import Control.Lens

-- | Modify the state given a lens, prism or traversal.
-- NB. This is 'Control.Lens.Zoom' for Notify.
type family Implanted m :: * -> *
class Implant m n s t | m -> s, n -> t, m t -> n, n s -> m where
    implant :: LensLike' (Implanted m) t s -> m -> n

-------------------------------------------------------------------------------
type family Dispatched m :: * -> *

-- | Changes the action type given a lens, prism or traversal
class Dispatch m n b a | m -> b, n -> a, m a -> n, n b -> m where
    dispatch :: LensLike' (Dispatched m) a b -> m -> n
