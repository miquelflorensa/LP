myLength :: [Int] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum (x:xs)
    | x > y = x
    | otherwise = y
    where y = myMaximum xs

suma :: [Int] -> Float
suma [] = fromIntegral 0
suma (x:xs) = fromIntegral x +  suma xs


average :: [Int] -> Float
average (x:xs) = (fromIntegral x +  suma xs) / fromIntegral (myLength(xs) + 1)

reversing :: [Int] -> [Int]
reversing [x] = [x]
reversing (x:xs) = (reverse xs) ++ [x]

buildPalindrome :: [Int] -> [Int]
buildPalindrome (x) = reversing x ++ (x)
-- Suposo que hi ha alguna forma de fer-ho tot en una funció però
-- no trobo la manera

remove :: [Int] -> [Int] -> [Int]
remove [] _ = []
remove (x:xs) y
    | elem x y = remove xs y
    | otherwise = x : remove xs y

flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x:xs) = x ++ flatten xs


oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens [] = ([],[])
oddsNevens (x:xs)
    | mod x 2 == 0 = (y, x : k)
    | otherwise = (x : y, k) 
    where (y,k) = oddsNevens xs 


isPrime :: Int -> Bool
isPrime n = isPrime' 2
    where 
        isPrime' d
            | n == d = True
            | mod n d == 0 = False
            | otherwise = isPrime'(d+1)

primeDivisors :: Int -> [Int]
primeDivisors n = primeDivisors' 2
    where
        primeDivisors' d
            | d == n = []
            | (isPrime d) && (mod n d) == 0 = d : x
            | otherwise = x
            where x = primeDivisors'(d+1)