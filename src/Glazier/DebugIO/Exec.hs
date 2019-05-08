module Glazier.DebugIO.Exec where

import Control.Monad.IO.Class
import Glazier.DebugIO
import Glazier.Command.Exec

execDebugIO ::
    ( MonadIO m
    )
    => (c -> m ()) -> DebugIO c -> m ()
execDebugIO executor (DebugIO m) = execIO executor m
