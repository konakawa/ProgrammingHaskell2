(||) :: Bool -> Bool -> Bool
True  || True  = True
True  || False = True
False || True  = True
False || False = False

(||) :: Bool -> Bool -> Bool
False || False = False
_     || _     = True

(||) :: Bool -> Bool -> Bool
False || b = b
True  || _ = True

(||) :: Bool -> Bool -> Bool
b1 || b2 = | b1 == b2  = b1
           | otherwise = True