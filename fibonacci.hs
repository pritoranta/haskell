-- the Nth element of the Fibonacci sequence
-- upper limit 5611 is set because of the limits of type Int
fibN :: Int -> Maybe Int
fibN n
    | n < 0 || n > 5611 = Nothing
    | otherwise = Just (fibSequence !! n)

-- USE CAREFULLY!
-- use fibN if you don't need a list
-- Fibonacci Sequence as an infinite list
fibSequence :: [Int]
fibSequence = 0 : 1 : zipWith (+) fibSequence (tail fibSequence)
