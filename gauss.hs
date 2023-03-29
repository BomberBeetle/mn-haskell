gauss :: [[Double]] -> [[Double]]
gauss lst 
    | length lst < 2 = lst
    | length (head lst) < 2 = lst
    | otherwise= pivot:(fmap (\x -> 0:x) (gauss $ fmap (tail) nullified))

    where pivot = findMax lst
          nullified = (nullifyFirstLayer pivot (removePivot pivot lst))

findMax :: [[Double]] -> [Double]
findMax lst = foldl1 (\prev x -> if (abs (head x)) > (abs (head prev)) then x else prev) lst

removePivot::Eq a => a->[a]->[a]
removePivot pivot (x:xs)
    | x == pivot = xs
    | otherwise = x:(removePivot pivot xs)

removePivot pivot [] = []

nullifyFirstLayer::Fractional a => [a] -> [[a]] -> [[a]]
nullifyFirstLayer pivot (x:xs) = (zipWith (\a pivotA -> a - (pivotA*multiFactor)) x pivot):(nullifyFirstLayer pivot xs)
    where multiFactor = head x / head pivot

nullifyFirstLayer pivot [] = []



