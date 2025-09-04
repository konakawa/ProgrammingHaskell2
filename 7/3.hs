map f = foldr (\x -> \l -> f x : l) []

filter p = foldr (\x -> \l -> if p x then x:l else l) []