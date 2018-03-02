{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.Core.Initializer where

import Control.Monad.Trans.Cont
import Control.Monad.Trans.Cont.Extras as TE
import Data.Diverse.Profunctor
import qualified Glazier.Core.Obj as Z

-- | handledBy (installs event listeners, etc) a widget
-- where the event are completely handled.

-- | handledBy something that fires an event @c@
type Initializer m s c = s -> ContT () m c
type ObjInitializer m v s c = Initializer m (Z.Obj v s) c

-- -- The identity for 'alsoInitializer''
-- nulInitializer' :: Initializer m r ()
-- nulInitializer' _ = pure ()

-- -- Activate left after the right.
-- -- The binary associative function for 'nulInitializer''.
-- alsoInitializer' :: Initializer m r () -> Initializer m r () -> Initializer m r ()
-- alsoInitializer' x y s = x s *> y s
-- infixr 6 `alsoInitializer'` -- like mappend

memptyInitializer :: forall c m s. Applicative m => Initializer m s c
memptyInitializer _ =  ContT $ const $ pure ()

mappendInitializer :: Applicative m => Initializer m s c -> Initializer m s c -> Initializer m s c
infixr 6 `mappendInitializer` -- like mappend
mappendInitializer f g s = f s `TE.alsoContT` g s

-- The identity for 'alsoInitializer'
noopInitializer :: Applicative m => Initializer m s ()
noopInitializer =  memptyInitializer @()

-- The identity for 'alsoInitializer'
nulInitializer :: Applicative m => Initializer m s (Which '[])
nulInitializer =  memptyInitializer @(Which '[])

-- Activate left after the right, firing results from both activators.
-- The binary associative function for 'nulInitializer'.
alsoInitializer ::
    ( Applicative m
    , ChooseBoth c1 c2 c3
    ) =>
    Initializer m r (Which c1)
    -> Initializer m r (Which c2)
    -> Initializer m r (Which c3)
infixr 6 `alsoInitializer` -- like mappend
alsoInitializer x y s = (diversify <$> x s) `TE.alsoContT` (diversify <$> y s)

-- Activate left after the right, firing only the result from the right.
-- The binary associative function for 'nulInitializer'.
thenInitializer :: Initializer m r () -> Initializer m r a -> Initializer m r a
thenInitializer x y s = x s *> y s
