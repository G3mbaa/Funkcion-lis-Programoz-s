--PN8G68
module Hazi12 where

parseCSV :: String -> [String]
parseCSV xs = case break (==';') xs of
    (ls, "") -> [ls]
    (ls, x:rs) -> ls : parseCSV rs

fixPoint :: Eq a => (a -> a) -> a -> a
fixPoint fv kezdo | fv kezdo /= kezdo = fixPoint fv (fv kezdo)
                  | otherwise = kezdo

isort :: Ord a => [a] -> [a]
isort ls = foldr insert [] ls

insert :: Ord a => a -> [a] -> [a]
insert e []     = [e]
insert e l@(x:xs)
  | e <= x     = e:l
  | otherwise = x : insert e xs