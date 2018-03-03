{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.Core.Obj where

import Control.Lens
import Data.IORef
import GHC.Generics

-- | naming convention:
--
-- foo :: Obj v s -> IO ()
-- foo this@(Z.Obj ref its) = do
--     obj <- readIORef ref
--     writeIORef ref (obj & its.bar .~ 5)
--     doSomethingElseWith this
data Obj v s = Obj (IORef v) (Lens' v s)

edit :: Lens' s a -> Obj v s -> Obj v a
edit l (Obj v i) = Obj v (i.l)

-- Polymorphic @Lens'@ prevents a auto derivied Generic instance
-- THe custom Generic instance uses 'ReifiedLens''
instance Generic (Obj v s) where
    from (Obj v s)
        = M1 (M1 (M1 (K1 v) :*: M1 (K1 (Lens s))))
    to (M1 (M1 (M1 (K1 v) :*: M1 (K1 (Lens s)))))
        = Obj v s
    type Rep (Obj v s) = D1
        ('MetaData
            "Obj"
            "Glazier.React.Framework.Obj"
            "glazier-react-widget"
            'False)
        (C1
            ('MetaCons
                "Obj"
                'PrefixI
                'True)
            (S1
                ('MetaSel
                    ('Just "ref")
                    'NoSourceUnpackedness
                    'NoSourceStrictness
                    'DecidedLazy)
                (Rec0 (IORef v))
                :*: S1
                    ('MetaSel
                        ('Just "its")
                        'NoSourceUnpackedness
                        'NoSourceStrictness
                        'DecidedLazy)
                    (Rec0 (ReifiedLens' v s))))

