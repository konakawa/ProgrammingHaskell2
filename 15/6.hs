sqroot :: Double -> Double
sqroot n =
  let next a = (a + n / a) / 2 in
  let l = iterate next 1.0 in
  head [b | (a, b) <- zip l (tail l), abs (a - b) < 0.00001 ]