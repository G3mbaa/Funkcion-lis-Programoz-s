module Hazi8 where
import Data.Maybe
import Data.List

modify :: (a -> Maybe a) -> [a] -> [a]
modify f [] = []
modify f (x:xs) | isJust (f x) == True = fromJust(f x):xs
                | otherwise = xs

strip' :: String -> String
strip' [] = []
strip' (x:xs) | x == '_' = strip' xs
             | x /= '_' = (x:xs)
strip :: String -> String
strip xs = strip' (reverse (strip' (reverse xs)))

average :: [Double] -> Double
average [] = 0
average ls = (sum ls) / fromIntegral (length ls)

coldestAvg :: [[Double]] -> Double
coldestAvg [x] = average x
coldestAvg (x:xs) | (average x) < minX = (average x)
                  | otherwise = minX
                  where minX = coldestAvg xs

reduce :: Eq a => [a] -> [a]
reduce xs = seged (group xs) where
    seged [] = []
    seged (x:xs) = (head x) : seged xs

listDiff :: Eq a => [a] -> [a] -> [a]
--listDiff xs ys = [ x | x <- xs, not $ x `elem` ys ]
--utóbbiban nem tudtam boldogulni
listDiff xs ys = concat (map (\x -> a x (not $ elem x ys)) xs)

a :: a -> Bool -> [a]
a x bool | bool == True = [x]
         | otherwise = []