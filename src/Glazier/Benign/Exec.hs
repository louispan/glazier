{-# LANGUAGE FlexibleContexts #-}

module Glazier.Benign.Exec where

import Control.Monad.IO.Class
import Glazier.Benign
import Glazier.Command.Exec

execBenignIO :: MonadIO m => (c -> m ()) -> Benign IO c -> m ()
execBenignIO executor m =  execIO executor (getBenign m)
