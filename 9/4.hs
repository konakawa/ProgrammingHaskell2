{-# LANGUAGE CPP #-}
#include "base.hs"

nums :: [Int]
nums = [1, 3, 7, 10, 25, 50]

n :: Int
n = length (concatMap exprs (choices nums))

nValid :: Int
nValid = length (filter (not . null . eval) (concatMap exprs (choices nums)))

main :: IO ()
main = do
  print n
  print nValid