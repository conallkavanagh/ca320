-- isPalindrome :: (Eq a)=> [a] -> Bool
-- isPalindrome [] = True
-- isPalindrome a = reverse a == a


isPalindrome :: (Eq a)=> [a] -> Bool
isPalindrome [] = True
isPalindrome [a] = True
isPalindrome a
    | head a == last a = isPalindrome $ init $ tail a
    | otherwise = False

shortest :: [[a]] -> [a]
shortest [a] = a
shortest (l:ls) 
    | length l < length (shortest ls) = l 
    | otherwise = shortest ls

