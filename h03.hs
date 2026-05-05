module Hazi3 where

f :: Double -> Double 
f x = x ^ 2 + 3 * x + 7

g :: [a] -> Double
g a = f (realToFrac (fromIntegral (length a)))

putIntoList :: a -> [a]
putIntoList x = [x]

headTail :: [a] -> (a, [a])
headTail a = (head a, (tail a))

doubleHead :: [a] -> [b] -> (a, b)
doubleHead a b = (head a, head b)

h1 :: (a, b, c) -> c
h1 (a,b,c) = c
--ez nem tiszta, ez a definíció ne szerepeljen, vagy a "last" függvény?

h2 :: (a, a -> b) -> b
h2 (a, b) = b a

h3 :: (b -> c, a -> b) -> a -> c
h3 (c, b) a = c (b a)