module Hazi3 where

f :: [[(a,[b])]] -> Int
f [(x,xs):[y,ys]]            = 0
f ([_]:[(x,[xs])]:[y,ys]:[]) = 1
f ([(x,y:_:[])]:[])          = 2

x0 = [[(1,[3]), (5, [7]),(9,[11])]]

x1 = [[('g', [666])],[('a', [1])],[('c', [1]), ('c', [1])]]

x2 = [[(1, [2, 3])]]

doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll (x:xs) = 2*x : doubleAll xs

isLonger :: [a] -> [b] -> Bool
isLonger [] _ = False
isLonger _ [] = True
isLonger (x:xs) (y:ys) = isLonger xs ys

lucas :: Int -> Int
lucas 1 = 2
lucas 2 = 1
lucas n = lucas(n-1) + lucas(n-2)

squareSum :: [Int] -> Int 
squareSum [] = 0
squareSum (x:xs) = x*x + squareSum xs