import Base hiding (expr, term, factor)

data Expr = Val Int | Add Expr Expr | Mul Expr Expr

expr :: Parser Expr
expr = do t <- term
          do symbol "+"
             e <- expr
             return (Add t e)
           <|> return t

term :: Parser Expr
term = do f <- factor
          do symbol "*"
             t <- term
             return (Mul f t)
           <|> return f

factor :: Parser Expr
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         <|> Val <$> natural