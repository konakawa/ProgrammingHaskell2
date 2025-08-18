last1 xs = head (reverse xs)

last2 xs = head (drop (length xs - 1) xs)

last3 xs = xs !! (length xs - 1)