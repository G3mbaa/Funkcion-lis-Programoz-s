module Hazi6 where
import Data.Maybe
import Data.List

value :: Eq a => a -> [(a,b)] -> Maybe b
value _ [] = Nothing
value a ((x,y):xs) | a == x = Just y
                   | otherwise = value a xs

insertPair :: Eq a => (a,b) -> [(a,b)] -> [(a,b)]
insertPair a [] = [a]
insertPair (a,b) ((x,y):xs) | a /= x = (x,y) : insertPair (a,b) xs
                            | otherwise = ((a,b):xs)

compress :: (Eq a, Integral b) => [a] -> [(b,a)]
compress xs = seged (group xs) where
    seged [] = []
    seged (x:xs) = (fromIntegral (length x), head x): seged xs

decompress :: (Eq a, Integral b) => [(b,a)] -> [a]
decompress [] = []
decompress ((x,y):xs) = replicate (fromIntegral x) y ++ decompress xs

format :: (Integral a) => a -> String -> String
format x xs | x <= fromIntegral (length xs) = xs
            | otherwise = [' '] ++ (format (x-1) xs)