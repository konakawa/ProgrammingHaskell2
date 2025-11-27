-- アプリカティブ則
-- pure id <*> x   = x
-- pure (g x)      = pure g <*> pure x
-- x <*> pure y    = pure (\g -> g y) <*> x
-- x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z


instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing  = Nothing
  fmap g (Just x) = Just (g x)

instance Applicative Maybe where
  -- pure :: a -> Maybe a
  pure = Just

  -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing  <*> _  = Nothing
  (Just g) <*> mx = fmap g mx

{-

prop: pure id <*> x = x

x = Nothing:
  pure id <*> Nothing
  (pure を適用)
= Just id <*> Nothing
  (<*> を適用)
= fmap id Nothing
  (fmap を適用)
= Nothing

x = Just x:
  pure id <*> Just x
  (pure, <*> を適用)
= fmap id (Just x)
  (fmap を適用)
= Just (id x)
  (id を適用)
= Just x


prop: pure (g x) = pure g <*> pure x

  pure (g x)
  (pure を適用)
= Just (g x)
  (fmap を逆適用)
= fmap g (Just x)
  (<*> を逆適用)
= Just g <*> Just x
  (pure を逆適用 x2)
= pure g <*> pure x


prop: x <*> pure y = pure (\g -> g y) <*> x

x = Nothing:
  Nothing <*> pure y
  (<*> を適用)
= Nothing
  (fmap を逆適用)
= fmap (\g -> g y) Nothing
  (<*> を逆適用)
= Just (\g -> g y) <*> Nothing
  (pure を逆適用)
= pure (\g -> g y) <*> Nothing

x = Just x:
  Just x <*> pure y
  (pure を適用)
= Just x <*> Just y
  (<*> を適用)
= fmap x (Just y)
  (fmap を適用)
= Just (x y)
= Just ((\g -> g y) x)
  (fmap を逆適用)
= fmap (\g -> g y) (Just x)
  (<*> を逆適用)
= Just (\g -> g y) <*> Just x
  (pure を逆適用)
= pure (\g -> g y) <*> Just x


prop: x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

(面倒なので省略します…)
(方針としては、いずれかが Nothing なら 両辺 Nothing になることと、、x, y, z が全て Just w の場合の証明をする)

-}