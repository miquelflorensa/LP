insert :: [Int] -> Int -> [Int]
insert [] n = [n]
insert (x:xs) n 
	| x > n = n : x : xs
	| otherwise = x : insert xs n

remove :: [Int] -> Int -> [Int]
remove [] _ = []
remove (x:xs) y
    | x == y = xs 
    | otherwise = x : remove xs y


merge :: [Int] -> [Int] -> [Int]
merge x [] = x
merge [] x = x
merge [] [] = []
merge (x:xs) (y:ys)
	| x > y = y : merge (x:xs) ys
	|otherwise = x : merge xs (y:ys)

{
qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) =
	let smallSort = qsort [a | a <- xs, a <= x];
		bigSort = qsort [a | a <- xs, a > x]
	in smallSort ++ [x] ++ bigSort

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 
-}