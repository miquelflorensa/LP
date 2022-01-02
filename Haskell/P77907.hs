absValue :: Integer -> Integer

absValue x
    | x >= 0 = x
    | otherwise = -x

power :: Integer -> Integer -> Integer

power x n = x ^ n

isPrime :: Integer -> Bool
isPrime n = isPrime' 2
    where 
        isPrime' d
            | n == d = True
            | mod n d == 0 = False
            | otherwise = isPrime'(d+1)

slowFib :: Integer -> Integer
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib(n-1) + slowFib(n-2)
