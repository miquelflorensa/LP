import Data.Char

flatten :: [[Int]] -> [Int]
flatten x = foldr (++) [] x 

-- myLength

myReverse :: [Int] -> [Int]
myReverse x = foldl (\acc x -> x:acc) [] x

myReverse2 :: [Int] -> [Int]
myReverse2 x = foldr (\x acc -> acc ++ [x]) [] x

countIn :: [[Int]] -> Int -> [Int]
countIn x y = foldl (\acc a -> acc ++ [foldl (\bcc b -> if(b == y) then (bcc+1) else bcc) 0 a]) [] x

firstWord :: String -> String
firstWord x = takeWhile (not.isSpace) (dropWhile (isSpace) x)