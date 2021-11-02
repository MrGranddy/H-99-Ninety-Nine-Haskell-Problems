isEqual :: Eq a => [a] -> [a] -> Bool
isEqual [] (y:_) = False
isEqual (x:_) [] = False
isEqual [] [] = True
isEqual (x:xs) (y:ys) = (x == y) && isEqual xs ys

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = isEqual xs (reverse xs)