-- the nth element of the fibonacci sequence
fib :: Int -> Int
fib n = case n of
 1 -> 1
 2 -> 1
 x -> (fib (x-1))+(fib (x-2))