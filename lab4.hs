myHead :: [a] -> a
myHead (x:xs) = x

myTail :: [a] -> [a]
myTail (x:xs) = xs

myAppend :: [a] -> [a] -> [a]
myAppend a b = a ++ b

myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

myInit :: [a] -> [a]
myInit [x,y] = [x]
myInit (x:xs) = [x] ++ (myInit xs)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength(xs)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

myConcat :: [[a]] -> [a]
myConcat [l] = l
myConcat (l:ls) = l ++ myConcat ls

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

myProduct :: Num a => [a] -> a
myProduct [x] = x
myProduct (x:xs) = x + mySum xs

myMaximum :: Ord a => [a] -> a
myMaximum [x] = x
myMaximum (x:xs)
    | x > myMaximum xs = x
    | otherwise = myMaximum xs

myElem :: Eq a => a -> [a] -> Bool
myElem a [] = False
myElem a (x:xs)
    | a == x = True
    | otherwise = myElem a xs

myDelete :: Eq a => a -> [a] -> [a]
myDelete a (x:xs)
    | a == x = xs
    | otherwise = [x] ++ (myDelete a xs)

myUnion :: Eq a => [a] -> [a] -> [a]
myUnion a [] = a
myUnion a (x:xs)
    | myElem x a = myUnion a xs
    | otherwise = myUnion (a ++ [x]) xs

myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect [] _ = []
myIntersect _ [] = []
myIntersect (x:xs) b
    | myElem x b = [x] ++ myIntersect xs b
    | otherwise = myIntersect xs b
