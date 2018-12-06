module Glazier.Benign
 ( Benign -- don't export constructor
 , getBenign
 , benignReadIORef
 ) where

import Data.IORef
import Control.Monad.Trans
import Glazier.Benign.Internal

benignReadIORef :: MonadIO m => IORef a -> Benign m a
benignReadIORef ref = Benign $ liftIO $ readIORef $ ref
