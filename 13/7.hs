import Base hiding (expr, term, factor)

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
           <|> do symbol "-"
                  e <- expr
                  return (t - e)
           <|> return t

term :: Parser Int
term = do p <- power
          do symbol "*"
             t <- term
             return (p * t)
           <|> do symbol "/"
                  t <- term
                  return (p `div` t)
           <|> return p

power :: Parser Int
power = do f <- factor
           do symbol "^"
              p <- power
              return (f ^ p)
           <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         <|> integer