data BinTree t = Empty | Root t (BinTree t) (BinTree t)
    deriving (Eq, Ord, Show)

addnode :: Ord a => a -> BinTree a -> BinTree a
addnode a Empty = (Root a Empty Empty)
addnode a (Root r left right)
    | a < r = Root r (addnode a left) right
    | otherwise = Root r left (addnode a right)

maketree :: Ord a => [a] -> BinTree a
maketree [] = Empty
maketree (x:xs) = addnode x (maketree xs)

inorder :: BinTree a -> [a]
inorder Empty = []
inorder (Root r left right) = (inorder left) ++ [r] ++ (inorder right)

mpsort :: Ord a => [a] -> [a]
mpsort a = inorder $ maketree a

hoaddnode :: Ord a => (a -> a -> Bool) -> a -> BinTree a -> BinTree a
hoaddnode _ a Empty = (Root a Empty Empty)
hoaddnode fn a (Root r left right)
    | fn a r = Root r (hoaddnode fn a left) right
    | otherwise = Root r left (hoaddnode fn a right)

homaketree :: Ord a => (a -> a -> Bool) -> [a] -> BinTree a
homaketree _ [] = Empty
homaketree fn (x:xs) = hoaddnode fn x (homaketree fn xs)

hosort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
hosort fn list = inorder ( homaketree fn list )
