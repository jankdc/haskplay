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

data BinaryTree a = EmptyTree
  | Leaf a
  | Branch a (BinaryTree a) (BinaryTree a)
  deriving (Show)

toList :: BinaryTree a -> [a]
toList n = toList' n [] where
  toList' EmptyTree lst      = lst
  toList' (Leaf v) lst       = v:lst
  toList' (Branch v l r) lst = toList' l (v:(toList' r lst))

insertTree :: Int -> BinaryTree Int -> BinaryTree Int
insertTree x EmptyTree = Leaf x
insertTree x (Leaf v)
  | x < v     = Branch v (Leaf x) EmptyTree
  | otherwise = Branch x (Leaf v) EmptyTree
insertTree x (Branch v l r)
  | x < v     = Branch v (insertTree x l) r
  | otherwise = Branch v l (insertTree x r)

preOrderTree :: BinaryTree a -> [a]
preOrderTree n = preOrderTree' n [] where
  preOrderTree' EmptyTree lst      = lst
  preOrderTree' (Leaf v) lst       = v:lst
  preOrderTree' (Branch v l r) lst = v : (preOrderTree' l (preOrderTree' r lst))

postOrderTree :: BinaryTree a -> [a]
postOrderTree n = postOrderTree' n [] where
  postOrderTree' EmptyTree lst      = lst
  postOrderTree' (Leaf v) lst       = v:lst
  postOrderTree' (Branch v l r) lst = (postOrderTree' l (postOrderTree' r lst)) ++ [v]

revList :: [a] -> [a]
revList lst = revList' lst [] where
  revList' [] lst     = lst
  revList' (x:xs) lst = revList' xs (x:lst)

revTree :: BinaryTree a -> BinaryTree a
revTree EmptyTree      = EmptyTree
revTree (Leaf v)       = Leaf v
revTree (Branch v l r) = Branch v (revTree r) (revTree l)
