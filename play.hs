inorder :: [Int] -> Bool
inorder [ ] = True
inorder [x] = True
inorder (x:y:xs) = x <= y && inorder (y:xs)

insert :: Int -> [Int] -> [Int]
insert e [] = [e]
insert e (x:xs)
  | e <= x    = e : (x:xs)
  | otherwise = x : (insert e xs)

sort :: [Int] -> [Int]
sort xs = sort' xs [] where
  sort' [] lst     = lst
  sort' (x:xs) lst = sort' xs (insert x lst)

