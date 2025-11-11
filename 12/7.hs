data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
              deriving Show

instance Functor Expr where
  -- fmap :: (a -> b) -> Expr a -> Expr b
  fmap f (Var x)     = Var (f x)
  fmap f (Val n)     = Val n
  fmap f (Add e1 e2) = Add (fmap f e1) (fmap f e2)

instance Applicative Expr where
  pure = Var

  -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  Var f   <*> x = fmap f x
  Val n   <*> _ = Val n
  Add f g <*> x = Add (f <*> x) (g <*> x)

instance Monad Expr where
  -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  Var x     >>= f = f x
  Val n     >>= _ = Val n
  Add e1 e2 >>= f = Add (e1 >>= f) (e2 >>= f)

-- >>= は変数を別の式に置換する -> 代入