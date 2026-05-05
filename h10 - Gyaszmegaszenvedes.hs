module Hazi10 where
import Data.Maybe

data List a = Nil | Cons a (List a)
    deriving (Eq)
instance Show a => Show (List a) where
    --show (Nil) = "<>"
    --show (Cons a Nil) = "<" ++ show a ++ ">"
    --show (Cons a (Cons xs Nil)) = "<" ++ show a ++ "," ++ show xs ++ ">"
    show = ('<' : ) . go
        where
            go Nil = ">"
            go (Cons a Nil) = show a ++ ">"
            go (Cons x xs) = show x ++ "," ++ go xs

head2 :: List a -> a
head2 (Cons x xs) = x    

tail2 :: List a -> List a
tail2 Nil = Nil
tail2 (Cons x xs) = xs

(+++) :: List a -> List a -> List a
(+++) Nil Nil = Nil
(+++) xs Nil = xs
(+++) Nil ys = ys
(+++) (Cons x xs) ys = (Cons x (xs +++ ys))

data Plant = Flower String Int | Tree String Int
    deriving (Show, Eq)

survive :: [Plant] -> Int -> [String]
survive [] x = []
survive ((Flower name int):xs) n | n < int = (survive xs n)
                               | n >= int = name : (survive xs n)
survive ((Tree name int):xs) n | n < int = (survive xs n)
                               | n >= int = name : (survive xs n)

avgTreeWater :: [Plant] -> Maybe Double
avgTreeWater [] = Nothing
avgTreeWater [Flower a b] = Nothing 
avgTreeWater xs = Just (treeSum xs / treeCount xs)

treeSum :: [Plant] -> Double
treeSum [] = 0
treeSum ((Tree name int):xs) = fromIntegral (int) + treeSum xs
treeSum ((Flower name int):xs) = treeSum xs

treeCount :: [Plant] -> Double
treeCount [] = 0
treeCount ((Flower a b):xs) = treeCount xs
treeCount ((Tree a b):xs) = 1 + treeCount xs