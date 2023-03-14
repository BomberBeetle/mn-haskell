import Numeric.AD

approxNTN::(Double -> Double) -> (Double -> Double) -> Double -> Maybe Double
approxNTN f f' x tol
    | abs (f xk) < tol = Just xk
    | f xk == 0 = Just xk
    | otherwise = approxNTN f f' xk tol
    where xk = x - (f x / f' x)
g x = sin x
g' = diff g

approxNTAuto:: (Double -> Double) -> Double -> Double -> Maybe Double
approxNTAuto f x tol = approxNTN f (diff f) x tol