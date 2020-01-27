type Queen = (Int,Int)

type Setup = [Queen]

-- author for function threatens:
-- Ville Tirronen, University of  Jyväskylä
threatens :: Queen -> Queen -> Bool
threatens (a1,a2) (b1,b2)

   | (a1,a2) == (b1,b2) = False -- Queen doesn't threaten herself

   | a2 == b2           = True  -- On the same row

   | a1 == b1           = True  -- On the same column

   | (a1-a2) == (b1-b2) = True  -- diagonal

   | (a1+a2) == (b1+b2) = True  -- diagonal

   | otherwise = False


--does any queen from the list threaten given queen
threatensFromList :: [Queen] -> Queen -> Bool
threatensFromList listQ q = any (threatens q) listQ

--does any queen from either list threaten any queen from the other list
threatenedLists :: [Queen] -> [Queen] -> Bool
threatenedLists a b = any (threatensFromList b) a

--all the solutions of column x (8 tall board)
column :: Int -> [Setup]
column x = [[(x,a)]|a<-[1..8]]

--combines fitting setups into bigger setups
combineFitting :: [Setup] -> [Setup] -> [Setup]
combineFitting a b = [s1++s2|s1<-a, s2<-b, not (threatenedLists s1 s2)]

{- solver for 8Queens problem, standard answer with call:

    queens [1..8]

   argument [Int] is the list of columns you want to utilize in the solutions:

   [a,b] gives all solutions with columns a and b in use -}
queens :: [Int] -> [Setup]
queens [] = []
queens [x] = column x
queens list = combineFitting (queens firstHalf) (queens secondHalf)
    where (firstHalf, secondHalf) = (take (div (length list) 2) list, drop (div (length list) 2) list)
