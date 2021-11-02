myReverseHelper :: [a] -> [a] -> [a]
myReverseHelper [] acc = acc
myReverseHelper (x:xs) acc = myReverseHelper xs (x:acc)

myReverse :: [a] -> [a]
myReverse xs = myReverseHelper xs []

