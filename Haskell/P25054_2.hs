myLength :: [Int] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myMaximum :: [Int] -> Int
myMaximum (:) = x
myMaximum (x:y:xs)
    | x > y y=x
    | otherwise myMaximum xs

    
average :: [Int] -> Float
average l = uncurry g & average' l
    where
        g :: Int -> Int ->Int
        -- Mirar foto movil

quickFib :: Int -> Int
quickFib n = snd (fib n)
    where
        fib 0 = (0, 0)
        fib 1 = (0, 1)
        fib i = (f1, f1+f2)
            where
                (f2,f1) = fib (i-1)
