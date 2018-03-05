-- {-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Glazier.Core.Handler where

-- import qualified Control.Arrow.Extras as E
import qualified Glazier.Core.Delegate as Z
import qualified Glazier.Core.Obj as Z

-- | Handle a input @a@ and fire a event @b@
type Handler s m a b = a -> Z.Delegate s m b
type ObjHandler ref v s m a b = Handler (Z.Obj ref v s) m a b

-- obviousHandler :: Handler s m a b -> Handler s m (Which '[a]) b
-- obviousHandler hdl = hdl . obvious

-- contramapHandler :: (a1 -> a2) -> Handler s m a2 b -> Handler s m a1 b
-- contramapHandler f hdl = hdl . f

-- mapHandler :: (b1 -> b2) -> Handler s m a b1 -> Handler s m a b2
-- mapHandler = fmap . fmap

-- memptyHandler :: Applicative m => Handler s m a b
-- memptyHandler = mempty

-- mappendHandler :: Applicative m => Handler s m a b -> Handler s m a b -> Handler s m a b
-- mappendHandler = mappend
-- infixr 6 `mappendHandler` -- like mappend

-- ignoreHandler :: forall a m s. Applicative m => Handler s m a ()
-- ignoreHandler = (const @_ @a) mempty

-- arrowHandler :: (a -> b) -> Handler s m a b
-- arrowHandler f = rk $ arr f

-- -- Chain the output from one handler into the input of the other.
--     Handler s m a b
--     -> Handler s m b c
--     -> Handler s m a c
-- -- intoH f g = f & E.rk2 (>>>) $ g
-- intoH f g = f >=> g

-- -- Chain the output from one handler into the input of the other
-- -- as much as possible. Any unhandled output is forwarded.
-- intoH' :: (Injected a2 b1 b2 b3)
--     => Handler s m a (Which b1)
--     -> Handler s m (Which a2) (Which b2)
--     -> Handler s m a (Which b3)
-- intoH' f g = f >=> E.underK1 injected g

-- -- Run th left handler and then the right handler with the same input,
-- -- and only fire events from the second input.
-- thenH :: Handler s m a () -> Handler s m a b -> Handler s m a b
-- thenH = ($*>!)

-- -- Run left and also the right handler with the same input, and combine the output type
-- -- A binary associative function for 'nulHandler'.
-- alsoH :: (Applicative m, ChooseBoth b1 b2 b3)
--     => Handler s m a (Which b1)
--     -> Handler s m a (Which b2)
--     -> Handler s m a (Which b3)
-- alsoH = liftA2 Z.also
-- infixr 6 `alsoH` -- like mappend

-- maybeH :: Applicative m
--     => Handler s m a b
--     -> Handler s m (Maybe a) b
-- maybeH hdl = maybe mempty hdl
