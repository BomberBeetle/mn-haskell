secantApprox f x xprev tol
    | abs (f x) < tol = x
    | otherwise = secantApprox f (x-(f x)*((x - xprev)/(f x - f xprev))) x tol

secantApproxSafe f x xprev tol depth max_depth
    | (abs (f x) < tol) || (depth >= max_depth) = x
    | otherwise = secantApproxSafe f (x-(f x)*((x - xprev)/(f x - f xprev))) x tol (depth+1) max_depth
    
secantApproxM f x xprev tol
    | abs (f x) < tol = do
        print x
        return x
    | otherwise = do
        print x
        secantApproxM f (x-(f x)*((x - xprev)/(f x - f xprev))) x tol