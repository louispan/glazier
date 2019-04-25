{-# LANGUAGE FlexibleContexts #-}

module Control.Monad.Benign.Exec where

import Control.Monad.Benign
import Control.Monad.IO.Class
import Glazier.Command.Exec

execBenignIO :: MonadIO m => (c -> m ()) -> Benign IO c -> m ()
execBenignIO executor m =  execIO executor (getBenign m)
