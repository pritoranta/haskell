import Data.List
import Data.Word

-- compresses a String into lengths of char runs, in order
encode :: String -> [(Char,Word8)]  
encode string = let
 chars = group string
 tuples = [enc c | c<-chars]
 in concat tuples
  where
   enc :: String -> [(Char, Word8)]
   enc string
    | length string <= 244 = [((head string),(fromIntegral (length string)))]
    | length string >244   = ((head string),(244)) : (enc (drop 244 string))

-- decodes a compressed String
decode :: [(Char,Word8)] -> String
decode list = concat (map dec list)
 where
  dec :: (Char, Word8) -> String
  dec (c,n) = take (fromEnum n) (repeat c)