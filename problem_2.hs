myButLast :: [a] -> a
myButLast [] = error "Input can't be empty"
myButLast [x] = error "Input must be at least a 2-list"
myButLast [x,y] = x
myButLast (_:xs) = myButLast xs