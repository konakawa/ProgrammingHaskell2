import Base
import qualified BaseCalc

eval :: String -> IO ()
eval xs =
  case parse expr xs of
    [(n, [])]  -> BaseCalc.calc (show n)
    [(n, out)] -> do
      showError xs out -- expr は式全体を消費するので、ここに来る時点で消費できなかった文字列が out に残っている
      BaseCalc.calc xs
    []         -> do
      showError xs ""
      BaseCalc.calc xs

showError :: String -> String -> IO ()
showError xs out = do
  let pos     = length xs - length out
      shown   = reverse (take 13 (reverse xs))
      off     = length xs - length shown
      relPos0 = pos - off
      relPos1 = max 0 relPos0
      relPos  = min relPos1 12
      caretLn = replicate relPos ' ' ++ "^"

  BaseCalc.display xs

  BaseCalc.writeat (3, 3) (replicate 13 ' ')
  BaseCalc.writeat (3, 3) (take 13 caretLn)

--   1. 13/baseCalc.hs の eval をこのファイルの eval に差し替え、
--      showError も 13/baseCalc.hs にコピペする
--   2. ghci -i13
--      :l 13/base.hs 13/baseCalc.hs
--      BaseCalc.run
