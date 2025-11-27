-- 関手則
-- fmap id      = id
-- fmap (g . h) = fmap g . fmap h

instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing  = Nothing
  fmap g (Just x) = Just (g x)

{-

prop: fmap id = id

  fmap id Nothing
  (fmap を適用)
= Nothing
  (id を逆適用)
= id Nothing

  fmap id (Just x)
  (fmap を適用)
= Just (id x)
  (id を適用)
= Just x
  (id を逆適用)
= id (Just x)


prop: fmap (g . h) = fmap g . fmap h

  fmap (g . h) Nothing
  (fmap を適用)
= Nothing
  (fmap を逆適用)
= fmap g Nothing
  (fmap を逆適用)
= fmap g (fmap h Nothing)

  fmap (g . h) (Just x)
  (fmap を適用)
= Just ((g . h) x)
= Just (g (h x))
  (fmap を逆適用)
= fmap g (Just (h x))
  (fmap を逆適用)
= fmap g (fmap h (Just x))

-}