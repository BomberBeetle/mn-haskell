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

solveForXs lst
    | detExtended triLst == 0 = Nothing
    | otherwise = Just (reverse $ innerSolve [] $ reverse lst) --Now I have to actually solve this.
    where triLst = gauss lst

innerSolve :: (Fractional a, Eq a)=> [a] -> [[a]] -> [a]
innerSolve coefs (x:xs) = innerSolve (coefs ++ [solvedVar]) xs
    where solvedVar = ((last x)- sum (zipWith (*) (take (length coefs) (clipZeros x)) coefs))/(reverse x !! 1)

innerSolve coefs [] = []

clipZeros (x:xs) = if x == 0 then clipZeros xs else x:xs
clipZeros [] = []

detExtended extMatrix = determinant $ fmap (init) extMatrix

--kibe abaixos

sPermutations :: [a] -> [([a], Int)]
sPermutations = flip zip (cycle [1, -1]) . foldl aux [[]]
  where
    aux items x = do
      (f, item) <- zip (cycle [reverse, id]) items
      f (insertEv x item)
    insertEv x [] = [[x]]
    insertEv x l@(y:ys) = (x : l) : ((y :) <$>) (insertEv x ys)

elemPos :: [[a]] -> Int -> Int -> a
elemPos ms i j = (ms !! i) !! j

prod
  :: Num a
  => ([[a]] -> Int -> Int -> a) -> [[a]] -> [Int] -> a
prod f ms = product . zipWith (f ms) [0 ..]

sDeterminant
  :: Num a
  => ([[a]] -> Int -> Int -> a) -> [[a]] -> [([Int], Int)] -> a
sDeterminant f ms = sum . fmap (\(is, s) -> fromIntegral s * prod f ms is)

determinant
  :: Num a
  => [[a]] -> a
determinant ms =
  sDeterminant elemPos ms . sPermutations $ [0 .. pred . length $ ms]
