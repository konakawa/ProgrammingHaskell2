bools :: [Bool] = [True]

nums :: [[Int]] = [[1]]

add :: Int -> Int -> Int -> Int = \x y z -> x + y + z

-- copy :: a -> (a, a) = \x -> (x, x)
copy :: a -> (a, a)
copy x = (x, x)

-- apply :: (a -> b) -> a -> b = \f x -> f x
apply :: (a -> b) -> a -> b
apply f x = f x

{-

bools や nums や add はこの記述で通るが、下二つはコメントアウトした記述だとエラーになる。

• You cannot bind scoped type variable ‘a’
    in a pattern binding signature
• In the pattern: copy :: a -> (a, a)
  In a pattern binding: copy :: a -> (a, a) = \ x -> (x, x)

ちょっと調べてみた感じ、そもそも Haskell は OCaml のようなインラインの型注釈を書かないっぽい？

add :: Int -> Int -> Int -> Int = \x y z -> x + y + z

が通るのは処理系の気持ちではどう見えているのか気になるが、この時点で深追いしてもどうにもならないので一旦先に進める。

ちなみに以下も通る

copy_int :: Int -> (Int, Int) = \x -> (x, x)

-}