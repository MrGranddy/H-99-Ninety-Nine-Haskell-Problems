data NestedList a = Elem a | List [NestedList a]

flattenHelper :: NestedList a -> [a] -> [a]
flattenHelper (Elem x) acc = x:acc
flattenHelper (List []) acc = acc
flattenHelper (List (x:xs)) acc = flattenHelper (List xs) (flattenHelper x acc)

flatten :: NestedList a -> [a]
flatten x = reverse $ flattenHelper x []