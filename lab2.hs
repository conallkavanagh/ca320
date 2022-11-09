isSum :: Int -> Int -> Int -> Bool
isSum a b c
    | a+b==c = True
    | a+c==b = True
    | b+c==a = True
    | otherwise = False


isTriangle :: Float -> Float -> Float -> Bool
isTriangle a b c
    | a+b<=c = False
    | a+c<=b = False
    | b+c<=a = False
    | otherwise = True

triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c  
    | isTriangle a b c == False = error "Not a Triangle"
    | isTriangle a b c == True = sqrt (s*(s-a)*(s-b)*(s-c))
            where s = (a+b+c)/2

