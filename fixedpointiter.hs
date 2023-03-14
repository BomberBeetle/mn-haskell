getTolerable::Float->[Float]->Float
getTolerable diff (x:y:xs)
    | abs(y - x) < diff = y
    | otherwise = getTolerable diff (y:xs)

getTolerable diff [x] = 1/0

getTolerable diff [] = 1/0

fixedPointIter f x0 tol = getTolerable tol (iterate f x0)

fpFindAt f x x0 tol = fixedPointIter f x0 tol
