module Glazier.Carry where

import qualified Control.Category as C
import Control.Arrow
import Data.Profunctor

newtype Carry r a b = Carry { runCarry :: (b -> r) -> (a -> r) }

instance Profunctor (Carry r) where
    dimap f g (Carry k) = Carry (\h a -> k (h . g) (f a))

instance Strong (Carry r) where
    first' (Carry k) = Carry (\h (a, c) -> k (\b -> h (b, c)) a)
    second' (Carry k) = Carry (\h (c, a) -> k (\b -> h (c, b)) a)

instance Choice (Carry r) where
    left' (Carry k) = Carry (\h a -> case a of
                                           Left a' -> k (h . Left) a'
                                           Right c -> h (Right c))
    right' (Carry k) = Carry (\h a -> case a of
                                           Right a' -> k (h . Right) a'
                                           Left c -> h (Left c))

instance C.Category (Carry r) where
    id = Carry Prelude.id
    (Carry bc) . (Carry ab) = Carry (\cr a -> ab (bc cr) a)

instance Arrow (Carry r) where
    arr f = Carry (. f)
    first (Carry k) = Carry (\h (a, c) -> k (\b -> h (b, c)) a)

instance ArrowChoice (Carry r) where
    left (Carry k) = Carry (\h a -> case a of
                                           Left a' -> k (h . Left) a'
                                           Right c -> h (Right c))
    right (Carry k) = Carry (\h a -> case a of
                                           Right a' -> k (h . Right) a'
                                           Left c -> h (Left c))

-- -- | The result of running a CPS computation with 'pure' as the
-- -- final continuation.
-- --
-- -- * @'evalCarryT' ('lift' m) = m@
-- evalCarryT :: (Applicative m) => CarryT b m a b -> a -> m b
-- evalCarryT m = runCarryT m pure
