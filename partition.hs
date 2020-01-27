{-
    parts a list into elements that complete given function and ones that don't
    example:
    partition even [1..10] ~> ([2,4,6,8,10],[1,3,5,7,9])
-}
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f []     = ([],[])
partition f x = ([a|a<-x, f a],[a|a<-x, not (f a)])
