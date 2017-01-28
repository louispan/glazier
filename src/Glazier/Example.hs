{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | This contains examples of general widget transformation functions.
module Glazier.Example where

import Control.Category
import Control.Lens
import Control.Monad.Reader
import Control.Monad.RWS.CPS hiding ((<>))
import Data.Foldable
import Data.List
import Data.Semigroup
import Glazier
import Glazier.Strict
import Prelude hiding (id, (.))

newtype Action a = Action { getAction :: a }
class AsAction s a | s -> a where
  _Action :: Prism' s (Action a)
instance AsAction (Action a) a where
  _Action = id

newtype ConsAction a = ConsAction { getConsAction :: a }
class AsConsAction s a | s -> a where
  _ConsAction :: Prism' s (ConsAction a)
instance AsConsAction (ConsAction a) a where
  _ConsAction = id

data Reset = Reset
class AsReset s where
  _Reset :: Prism' s Reset
instance AsReset Reset where
  _Reset = id

data Tail = Tail
class AsTail s where
  _Tail :: Prism' s Tail
instance AsTail Tail where
  _Tail = id

newtype Set a = Set  { getSet :: a }
class AsSet s a | s -> a where
  _Set :: Prism' s (Set a)
instance AsSet (Set a) a where
  _Set = id

-- | Transforms a widget into an optional widget.
-- This wraps the original model inside a Maybe.
-- The new action is now a sum type that contains the additional actions:
-- * A Reset action
-- * A Set action
-- * A mapping action
-- * The original action
-- The original action is wrapped using the given prism and will only
-- modify the state if the preview of the prism is not Nothing.
-- The view will be mempty if the model is Nothing.
-- Widget was a w s m c v
-- Widget s v m a c
optionalExample ::
  ( Monoid v
  , Monoid c
  , Semigroup v
  , Semigroup c
  , AsSet a s
  , AsReset a
  , AsAction a (Maybe s -> Maybe s)
  , Monad m
  )
  => Prism' a a' -> Widget s v m a' c -> Widget (Maybe s) v m a c
optionalExample p w =
     (
     implant _Just -- original update will only work if model is Just
     >>> dispatch p -- make original action part of a smaller action, in preparation of adding other actions below
     ) w
  <> statically mempty -- change mempty to specify a rendering function when Nothing
  <> dynamically
    (  dispatch _Set    (review _GadgetT $ \a _ -> pure (mempty,Just $ getSet a))
    <> dispatch _Action (review _GadgetT $ \(Action f) s -> pure (mempty, f s))
    <> dispatch _Reset  (review _GadgetT $ \_ _ -> pure (mempty, Nothing))
    )

-- | Transforms a widget into an list widget.
-- Given a separator rendering widget, and a widget,
-- this wraps the original model inside a list.
-- The new action is now a sum type that contains the additional actions:
-- * A Tail action
-- * A Cons action
-- * A mapping action
-- * The original action
-- The original action is wrapped using the given prism and will only
-- modify the state of the head.
-- The view will be mempty if Nil.
listExample ::
  ( Monoid v
  , Monoid c
  , Semigroup v
  , Semigroup c
  , AsTail a
  , AsConsAction a s
  , AsAction a ([s] -> [s])
  , Monad m
  )
  => Prism' b a -> Widget s v m a c -> Widget [s] v m b c
listExample p (Widget (Window d) u) =
     -- Create a list rendering function by
     -- interspercing the separator with the View from the original widget.
     statically (Window . ReaderT $ \ss -> do
                        ss' <- traverse (runReaderT d) ss
                        pure (fold $ intersperse separator ss'))
  <> dynamically
    (  implant (ix 0) u -- original update will only work on the head of list
    <> dispatch _Tail       (review _GadgetT $ \_ s -> pure (mempty, tail s))
    <> dispatch _ConsAction (review _GadgetT $ \(ConsAction a) s -> pure (mempty, a : s))
    <> dispatch _Action     (review _GadgetT $ \(Action f) s -> pure (mempty, f s))
    )
  & dispatch p -- make original action part of a smaller action
 where separator = mempty -- change mempty to specify a rendering function

-- | Transforms a widget into an dictionary widget.
-- Given a ordering function, a key function, and a separator rendering function,
-- allows a dictionary of k to Widget.
-- The new action is now a sum type that contains the additional actions:
-- * A mapping action
-- * A tuple of (key, original action)
-- The original action is now a tuple with an additional key, which will act on the widget if the key exists in the map.
indexedExample ::
  ( Monoid v
  , Monoid c
  , Field2 b b a a
  , Field1 b b (Index (t s)) (Index (t s))
  , Ixed (t s)
  , Semigroup v
  , Semigroup c
  , AsAction b (t s -> t s)
  , IxValue (t s) ~ s
  , Monad m
  , Traversable t
  )
  => Widget s v m a c -> Widget (t s) v m b c
indexedExample (Widget (Window d) g) =
     -- Create a rendering function by folding the original view function
     statically (Window . ReaderT $ \ss -> do
                        ss' <- traverse (runReaderT d) ss
                        pure (fold ss'))
  <>
    dynamically
    (
       -- This effectively dispatches the Update
       -- ie the action type has changed
       -- so a @dispatch prism@ is not required
       (do
         x <- ask
         let k = x ^. _1
             -- a = x ^. _2
         -- run u but for a state implanted by ix k
         zoom (ix k) (magnify _2 g)
       )
    <>
      dispatch _Action     (review _GadgetT $ \(Action f) s -> pure (mempty, f s))
    )
