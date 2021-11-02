myLast :: [a] -> a
myLast [] = error "Input can't be empty"
myLast [x] = x
myLast (_:xs) = myLast xs