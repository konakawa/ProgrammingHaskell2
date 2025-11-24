import Base hiding (expr, term, factor)

{-

この問題では減算のみ

expr ::= expr - int | int
int  ::= 0 | 1 | 2 | ...

-}

badExpr :: Parser Int
badExpr = do e <- badExpr
             symbol "-"
             i <- natural
             return (e - i)
           <|> natural

-- ↑の実装では badExpr が無限再帰ループに入る
-- many と foldl を使って解決する

expr :: Parser Int
expr = do n  <- natural
          ns <- many (do symbol "-"
                         natural)
          return (foldl (-) n ns)