{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.Core.Obj where

import Control.Lens
import GHC.Generics

-- | naming convention:
--
-- foo :: Obj IORef v s -> IO ()
-- foo this@(Z.Obj ref its) = do -- or use RecordWildcards
--     obj <- readIORef ref
--     writeIORef ref (obj & its.bar .~ 5)
--     doSomethingElseWith this
data Obj ref v s = Obj { ref :: ref v,  its :: Lens' v s }

-- | Tip: This can be used to 'magnify' 'MonadReader' with
-- @magnify ('to' ('edit' theLens)) theReader@
edit :: Lens' s a -> Obj ref v s -> Obj ref v a
edit l (Obj v i) = Obj v (i.l)

-- Polymorphic @Lens'@ prevents a auto derivied Generic instance
-- THe custom Generic instance uses 'ReifiedLens''
instance Generic (Obj ref v s) where
    from (Obj v s)
        = M1 (M1 (M1 (K1 v) :*: M1 (K1 (Lens s))))
    to (M1 (M1 (M1 (K1 v) :*: M1 (K1 (Lens s)))))
        = Obj v s
    type Rep (Obj ref v s) = D1
        ('MetaData
            "Obj"
            "Glazier.React.Core.Obj"
            "glazier"
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
                (Rec0 (ref v))
                :*: S1
                    ('MetaSel
                        ('Just "its")
                        'NoSourceUnpackedness
                        'NoSourceStrictness
                        'DecidedLazy)
                    (Rec0 (ReifiedLens' v s))))

