-- halves a list and returns the halves in a tuple
deal :: [a] -> ([a],[a])
deal list = (take half list,drop half list)
    where half = div (length list) 2

-- merges two ascendingly ordered lists
-- into a single list in ascending order
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs)(y:ys)
 | x < y = x : merge xs (y:ys)
 | otherwise = y : merge (x:xs) ys

-- merge sort
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list = merge (mergeSort firstHalf) (mergeSort secondHalf)
    where (firstHalf,secondHalf) = deal list