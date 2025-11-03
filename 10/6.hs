import System.IO

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

readLine :: IO String
readLine = readLine' ""
  where
    readLine' :: String -> IO String
    readLine' acc = do
      c <- getCh
      case c of
        '\n' -> do putChar '\n'
                   return (reverse acc)
        '\DEL' -> if null acc
                  then readLine' acc
                  else do putStr "\b \b"
                          readLine' (tail acc)
        _ -> do putChar c
                readLine' (c : acc)
