--PN8G68
module Hazi5 where

mountain :: Int -> [Int]
mountain 0 = [0]
mountain n 
    | n `mod` 2 == 0 = [0, 2..n] ++ [n-2, n-4..0]
    | otherwise  = [0, 2..n] ++ [n-3, n-5..0]

insertAt :: Int -> a -> [a] -> [a]
insertAt _ f [] = [f]
insertAt 0 f xs = f:xs
insertAt n f (x:xs) 
    | n <= 0 = (f:x:xs)
    | otherwise = x : insertAt (n-1) f xs

positiveProduct :: (Num a, Ord a) => [a] -> a
positiveProduct [] = 1
positiveProduct (x:xs)
    | x > 0 = x * positiveProduct xs
    | otherwise = positiveProduct xs

validGame :: String -> Bool
validGame [] = True
validGame (x:' ':z:xs) = (x==z) && validGame (z:xs)
validGame (x:xs) = validGame xs


seged :: [a] -> [a]
seged [] = []
seged [a] = [a]
seged (x:xs) = head xs : x : tail xs

swapElems :: [[a]] -> [[a]]
swapElems [] = []
--swapElems [a] = [a]
swapElems (x:xs) = seged x : swapElems xs