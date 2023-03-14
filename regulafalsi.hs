regulaFalsi f a b tol 
    | (f a)*f ck < 0 = approxNTN f a ck tol
    | (f b)*f ck < 0 = approxNTN f ck b tol
    | otherwise = Nothing
    where ck = lerp f a b

approxNTN :: (Ord a, Fractional a) => (a -> a) -> a -> a -> a -> Maybe a
approxNTN f a b tol
    | (f a)*(f b) > 0 = Nothing
    | abs (f ck) < tol = Just ck
    | f ck == 0 = Just ck
--    | abs (b-a) < tol = Just ck
    | otherwise = regulaFalsi f a b tol
    where ck = lerp f a b

lerp f a b = ((f b)*a - (f a)*b)/((f b) - (f a))
