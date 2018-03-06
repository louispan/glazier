{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.Core.Obj where

import Control.Lens
import GHC.Generics

-- | naming convention:
-- foo this@(Z.Obj self my) = do -- or use RecordWildcards
--     me <- readIORef self
--     writeIORef ref (me & my.bar .~ 5)
--     doSomethingElseWith this
data Obj ref parent a = Obj { self :: ref parent, my :: Lens' parent a }

-- | Tip: This can be used to 'magnify' 'MonadReader' with
-- @magnify ('to' ('access' theLens)) theReader@
access :: Lens' s a -> Obj ref p s -> Obj ref p a
access l (Obj t s) = Obj t (s.l)

-- accessor :: Lens' s a -> Lens' (Obj ref p s) (Obj ref p a)
-- accessor l = lens (\(Obj p s) -> Obj p (s.l))
--     (\(Obj _ s) (Obj p _) -> Obj p s)

-- Polymorphic @Lens'@ prevents a auto derivied Generic instance
-- THe custom Generic instance uses 'ReifiedLens''
instance Generic (Obj ref p a) where
    from (Obj p a)
        = M1 (M1 (M1 (K1 p) :*: M1 (K1 (Lens a))))
    to (M1 (M1 (M1 (K1 p) :*: M1 (K1 (Lens a)))))
        = Obj p a
    type Rep (Obj ref p a) = D1
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
                    ('Just "this")
                    'NoSourceUnpackedness
                    'NoSourceStrictness
                    'DecidedLazy)
                (Rec0 (ref p))
                :*: S1
                    ('MetaSel
                        ('Just "my")
                        'NoSourceUnpackedness
                        'NoSourceStrictness
                        'DecidedLazy)
                    (Rec0 (ReifiedLens' p a))))

