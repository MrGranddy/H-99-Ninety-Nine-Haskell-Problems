elementAt :: [a] -> Int -> a
elementAt [] _ = error "The list has the wrong size"
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n - 1)
