luhnDouble :: Int -> Int
luhnDouble n = n * 2 `mod` 9

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = 
  let [a', b', c', d'] = map luhnDouble [a, b, c, d] in
  (a + b + c + d) `mod` 10 == 0