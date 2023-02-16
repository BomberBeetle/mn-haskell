func3 x = (x*x*x) - 3*x - 1

bifurcate :: (Float -> Float) -> Float -> Float -> Float -> Maybe Float
bifurcate f a b error
    | (f a)*f ((a+b)/2) < 0 = approxRoot f a ((a+b)/2) error
    | (f b)*f ((a+b)/2) < 0 = approxRoot f ((a+b)/2) b error
    | otherwise = Nothing

approxRoot :: (Float -> Float) -> Float -> Float -> Float -> Maybe Float
approxRoot f a b error
   | (f a)*(f b) > 0 = Nothing
   | (f ((a+b)/2)) == 0 = Just ((a+b)/2)
   | abs (b-a) < error = Just ((a+b)/2)
   | otherwise = bifurcate f a b error