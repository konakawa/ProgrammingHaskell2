luhnDouble :: Int -> Int
luhnDouble n = if n * 2 > 9 then n * 2 - 9 else n * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = 
  let [a', b', c', d'] = map luhnDouble [a, b, c, d] in
  (a' + b' + c' + d') `mod` 10 == 0