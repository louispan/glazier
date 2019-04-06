{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Glazier.Command.Exec where

import Data.Kind
import Control.Applicative
import Control.Lens
import Control.Monad.IO.Unlift
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Maybe.Extras
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Foldable
import Data.Proxy
import Glazier.Command
import qualified UnliftIO.Concurrent as U

-- | type function to get the list of effects in a @cmd@, parameterized over @c@
type family CmdTypes cmd c :: [Type]

-- | A command type that removes the @IO c@ from the @CmdTypes@ of the input @c@
newtype NoIOCmd c = NoIOCmd { unNoIOCmd :: Which (CmdTypes (NoIOCmd c) (NoIOCmd c)) }

-- | Removes the @IO c@ from the @CmdTypes@ of the input @c@
type instance CmdTypes (NoIOCmd cmd) c = Remove (IO c) (CmdTypes cmd c)

-- UndecidableInstances!
instance (AsFacet a (Which (CmdTypes (NoIOCmd c) (NoIOCmd c)))) => AsFacet a (NoIOCmd c) where
    facet = iso unNoIOCmd NoIOCmd . facet

-- | Create an executor for a variant in the command type.
-- returns a 'Proxy' to keep track of the the types handled by the executor.
maybeExec :: (Applicative m, AsFacet a c) => (a -> m b) -> c -> MaybeT m (Proxy '[a], b)
maybeExec k c = MaybeT . sequenceA $ (fmap (\b -> (Proxy, b)) . k) <$> preview facet c

-- | Tie an executor with itself to get the final interpreter
fixExec :: Functor m => ((c -> m ()) -> c -> MaybeT m (Proxy cs, ())) -> c -> m (Proxy cs, ())
fixExec fexec = (`evalMaybeT` (Proxy, ())) . fexec (fmap snd . fixExec fexec)

-- | A variation of 'fixExec' for executors that return cmds that should be evaluated last
fixExec' :: Monad m => ((c -> m ()) -> c -> MaybeT m (Proxy cs, [c])) -> c -> m (Proxy cs, ())
fixExec' fexec c = do
    let go = (`evalMaybeT` []) . fmap snd . fexec (fmap snd . fixExec' fexec)
        gos cs = do
            case cs of
                [] -> pure ()
                cs' -> do
                    cs'' <- (DL.concat . (fmap DL.fromList)) <$> traverse go cs'
                    gos $ DL.toList cs''
    cs <- go c
    gos cs
    pure (Proxy, ())


-- | Use this function to verify at compile time that the given executor will fullfill
-- all the variant types in a command type.
-- redundant-constraints: used to constrain xs and ys
verifyExec ::
    ( AppendUnique '[] ys ~ ys
    , AppendUnique xs ys ~ xs
    , AppendUnique ys xs ~ ys
    , Functor m
    )
    => (c -> Which xs) -> (c -> m (Proxy ys, b)) -> (c -> m b)
verifyExec _ g = fmap snd .  g

-- 'verifyExec' and 'fixExec' an executor.
verifyAndFixExec ::
    ( AppendUnique '[] ys ~ ys
    , AppendUnique xs ys ~ xs
    , AppendUnique ys xs ~ ys
    , Functor m
    ) => (c -> Which xs) -> ((c -> m ()) -> c -> MaybeT m (Proxy ys, ())) -> c -> m ()
verifyAndFixExec unCmd maybeExecuteCmd = verifyExec unCmd (fixExec maybeExecuteCmd)

-- 'verifyExec' and 'fixExec'' an executor.
verifyAndFixExec' ::
    ( AppendUnique '[] ys ~ ys
    , AppendUnique xs ys ~ xs
    , AppendUnique ys xs ~ ys
    , Monad m
    ) => (c -> Which xs) -> ((c -> m ()) -> c -> MaybeT m (Proxy ys, [c])) -> c -> m ()
verifyAndFixExec' unCmd maybeExecuteCmd = verifyExec unCmd (fixExec' maybeExecuteCmd)

-- | Combines executors, keeping track of the combined list of types handled.
-- redundant-constraints: used to constrain a''
orMaybeExec :: (Monad m, a'' ~ Append a a') => MaybeT m (Proxy a, b) -> MaybeT m (Proxy a', b) -> MaybeT m (Proxy a'', b)
orMaybeExec m n = (\b -> (Proxy, b)) <$> ((snd <$> m) <|> (snd <$> n))
infixl 3 `orMaybeExec` -- like <|>

execConcur ::
    MonadUnliftIO m
    => (c -> m ())
    -> Concur c a
    -> m a
execConcur executor (Concur m) = do
        ea <- execConcur_ executor
        -- Now run the possibly blocking io
        liftIO $ either id pure ea
  where
    execConcur_ executor' = do
        -- get the list of commands to run
        (ma, cs) <- liftIO $ unNewEmptyMVar $ runStateT m mempty
        -- run the batched commands in separate threads
        traverse_ (void . U.forkIO . executor') (DL.toList cs)
        pure ma

execIO :: MonadIO m => (c -> m ()) -> IO c -> m ()
execIO executor m = liftIO m >>= executor
