module Hazi7 where 
import Data.Maybe
import Data.List

compress :: (Eq a, Integral b) => [a] -> [(b, a)]
compress ls = map (\lt -> (fromIntegral (length lt), head lt)) (group ls)

decompress :: (Eq a, Integral b) => [(b, a)] -> [a]
decompress xs = concat (map (\(c, n) -> replicate (fromIntegral c) n) (xs))

catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (x:xs) | isNothing x == True = catMaybes' xs
                  | otherwise = (fromJust x) : catMaybes' xs

allWithFoldr :: (a -> Bool) -> [a] -> Bool
allWithFoldr fv xs = foldr f True xs
    where f x ls = fv x && ls