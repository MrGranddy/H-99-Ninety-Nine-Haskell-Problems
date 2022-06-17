data EncodedItem = Single Char | Multiple Int Char
    deriving Show

pack :: Eq a => [a] -> [[a]]
pack inputList = reverse $ helper inputList [] []
    where
        helper :: Eq a => [a] -> [a] -> [[a]] -> [[a]]
        helper [] acc1 acc2 = []
        helper [x] [] acc2 = [x]:acc2
        helper [x] acc1 acc2 = (x:acc1):acc2
        helper (x:y:rest) acc1 acc2 =
            if x == y
            then helper (y:rest) (x:acc1) acc2
            else helper (y:rest) [] ((x:acc1):acc2)

encode :: Eq a => [a] -> [(Int, a)]
encode inputList = map (\x -> (length x, head x)) (pack inputList) 

modifier :: (Int, Char) -> EncodedItem
modifier (1, l) = Single l
modifier (n, l) = Multiple n l 

encodeModified :: [Char] -> [EncodedItem]
encodeModified inputList = map modifier (encode inputList)