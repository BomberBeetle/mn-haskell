secantApprox f x xprev tol
    | abs (f x) < tol = x
    | otherwise = secantApprox f (x-(f x)*((x - xprev)/(f x - f xprev))) x tol