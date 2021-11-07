compressHelper :: Eq a => [a] -> [a] -> [a]
compressHelper [] acc = []
compressHelper [x] acc = x:acc
compressHelper (x1:x2:xs) acc = if x1 == x2 then compressHelper (x2:xs) acc else compressHelper (x2:xs) (x1:acc)

compress :: Eq a => [a] -> [a]
compress x = reverse $ compressHelper x []