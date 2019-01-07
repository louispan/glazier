
-- #############################################################################
-- OLD
-- #############################################################################

-- -----------------------------------------------------
-- MToJSON1, MFromJSON1 Instances
-- -----------------------------------------------------


instance Applicative m => MToJSON1 m Maybe where
    mliftToEncoding t _ (Just a) = t a
    mliftToEncoding _  _ Nothing  = pure null_

-- instance Applicative m => MFromJSON1 m Maybe where
--     mliftParseJSON _ _ Null = pure (pure Nothing)
--     mliftParseJSON p _ a    = fmap Just <$> p a

-- instance (Applicative m, ToJSONKey k) => MToJSON1 m (M.Map k) where
--     mliftToEncoding g _ = case toJSONKey of
--         ToJSONKeyText _ f -> mdictEncoding f g M.foldrWithKey
--         ToJSONKeyValue _ f -> mlistEncoding (pairEncoding f) . M.toList
--       where
--         pairEncoding f (a, b) = mlistEncoding id [pure $ f a, g b]

instance (Applicative m, FromJSONKey k, Ord k) => MFromJSON1 m (M.Map k) where
    mliftParseJSON p _ = case fromJSONKey of
        FromJSONKeyCoerce _ -> withObject "Map k v" $
            H.foldrWithKey (\k v m -> getCompose $ M.insert
                <$> (Compose $ (pure . pure $ unsafeCoerce k) <?> Key k)
                <*> (Compose $ p v <?> Key k)
                <*> Compose m)
                (pure $ pure M.empty)

        FromJSONKeyText f -> withObject "Map k v" $
            H.foldrWithKey (\k v m -> getCompose $ M.insert
                <$> (Compose $ (pure . pure $ f k) <?> Key k)
                <*> (Compose $ p v <?> Key k)
                <*> Compose m)
                (pure $ pure M.empty)

        FromJSONKeyTextParser f -> withObject "Map k v" $
            H.foldrWithKey (\k v m -> getCompose $ M.insert
                <$> (Compose $ (pure <$> f k) <?> Key k)
                <*> (Compose $ p v <?> Key k)
                <*> Compose m)
                (pure $ pure M.empty)

        FromJSONKeyValue f -> withArray "Map k v" $ \arr ->
            getCompose $ M.fromList <$> (
                -- [Compose (a, b)] -> Compose [(a, b)]
                sequenceA
                . fmap Compose
                -- [Parser m (a, b)]
                . zipWith (_parseIndexedJSONPair ((<$>) . (,)) f p) [0..] . V.toList $ arr)


-- -----------------------------------------------------
-- MToJSON, MFromJSON Instances
-- -----------------------------------------------------

instance Applicative m => MToJSON m Bool where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}

instance Applicative m => MFromJSON m Bool where
    mparseJSON = fmap pure . parseJSON

instance Applicative m => MToJSON m Char where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}

instance Applicative m => MFromJSON m Char where
    mparseJSON = fmap pure . parseJSON

instance Applicative m => MToJSON m Text where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}

instance Applicative m => MFromJSON m Text where
    mparseJSON = fmap pure . parseJSON
    {-# INLINE mparseJSON #-}

instance Applicative m => MToJSON m Scientific where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}

instance Applicative m => MFromJSON m Scientific where
    mparseJSON = fmap pure . parseJSON

instance Applicative m => MToJSON m Value where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}

instance Applicative m => MFromJSON m Value where
    mparseJSON = fmap pure . parseJSON

instance Applicative m => MToJSON m Int where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}

instance Applicative m => MFromJSON m Int where
    mparseJSON = fmap pure . parseJSON

instance Applicative m => MToJSON m Array where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}

instance Applicative m => MToJSON m Object where
    mtoEncoding = pure . toEncoding
    {-# INLINE mtoEncoding #-}

instance (MToJSON m a, ToJSONKey k) => MToJSON m (M.Map k a) where
    mtoEncoding = mtoEncoding1

instance (MFromJSON m a, FromJSONKey k, Ord k) => MFromJSON m (M.Map k a) where
    mparseJSON = mparseJSON1

instance (MToJSON m a) => MToJSON m (Maybe a) where
    mtoEncoding = mtoEncoding1

instance (MFromJSON m a) => MFromJSON m (Maybe a) where
    mparseJSON = mparseJSON1
