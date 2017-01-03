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
import Data.Foldable
import Data.List
import Data.Semigroup
import Glazier
import Prelude hiding (id, (.))
-- import Control.Lens.Classy
-- import Data.Proxy.Lens.Classy
-- import Data.Tagged.Lens.Classy

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
optionalExample ::
  ( Monoid d
  , Monoid c
  -- , AsProxy a "Reset"
  -- , AsTagged a "Map" (Maybe a2 -> Maybe a2)
  -- , AsPrism a a1
  , Semigroup d
  , Semigroup c
  , AsSet a a2
  , AsReset a
  , AsAction a (Maybe a2 -> Maybe a2)
  )
  => Prism' a a1 -> Widget a1 a2 c d -> Widget a (Maybe a2) c d
optionalExample p w =
     (
     implant _Just -- original update will only work if model is Just
     >>> dispatch p -- make original action part of a smaller action, in preparation of adding other actions below
     ) w
  <> statically mempty -- change mempty to specify a rendering function when Nothing
  <> dynamically
    (  dispatch _Set      (mkNotifyRS' $ const . Just . getSet)
    <> dispatch _Action   (mkNotifyRS' getAction)
    -- <> dispatch (_Tagged (Proxy :: Proxy "Map")) (updateRS' unTagged)
    <> dispatch _Reset    (mkNotifyRS' . const $ const Nothing)
    -- <> dispatch (_Proxy (Proxy :: Proxy "Reset")) (updateRS' . const $ const Nothing)
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
  ( Monoid d
  , Monoid c
  , Semigroup d
  , Semigroup c
  -- , AsPrism b a
  -- , AsProxy a "Tail"
  , AsTail a
  -- , AsTagged a "Cons" s
  , AsConsAction a s
  -- , AsTagged a "Map" ([s] -> [s])
  , AsAction a ([s] -> [s])
  )
  => Prism' b a -> Widget a s c d -> Widget b [s] c d
listExample p (Widget u (Depict d)) =
     -- Create a list rendering function by
     -- interspercing the separator with the View from the original widget.
     statically (Depict $ \ss -> fold $ intersperse separator $ fmap d ss)
  <> dynamically
    (  implant (ix 0) u -- original update will only work on the head of list
    -- <> dispatch (_Proxy (Proxy :: Proxy "Tail")) (updateRS' . const $ tail)
    <> dispatch _Tail   (mkNotifyRS' . const $ tail)
    -- <> dispatch (_Tagged (Proxy :: Proxy "Cons")) (updateRS' $ (:) . unTagged)
    <> dispatch _ConsAction (mkNotifyRS' $ (:) . getConsAction)
    -- <> dispatch (_Tagged (Proxy :: Proxy "Map")) (updateRS' unTagged)
    <> dispatch _Action (mkNotifyRS' getAction)
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
  ( Functor t
  , Foldable t
  , Monoid d
  , Monoid c
  , Field2 b b a a
  , Field1 b b (Index (t s)) (Index (t s))
  , Ixed (t s)
  , Semigroup d
  , Semigroup c
  -- , AsPrism b a
  -- , AsTagged b "Map" (t s -> t s)
  , AsAction b (t s -> t s)
  , IxValue (t s) ~ s
  )
  => Widget a s c d -> Widget b (t s) c d
indexedExample (Widget (Notify u) (Depict d)) =
     -- Create a rendering function by folding the original view function
    statically (Depict $ \ss -> fold (fmap d ss))
  <>
    dynamically
    (
       -- This effectively dispatches the Update
       -- ie the action type has changed
       -- so a @dispatch prism@ is not required
       (Notify $ do
         x <- ask
         let k = x ^. _1
             a = x ^. _2
         -- run u but for a state implantded by ix k
         lift $ zoom (ix k) (runReaderT u a)
       )
    <>
      -- dispatch (_Tagged (Proxy :: Proxy "Map")) (updateRS' unTagged)
      dispatch _Action (mkNotifyRS' getAction)
    )
