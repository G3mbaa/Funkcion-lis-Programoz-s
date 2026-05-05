module Hazi9 where

data Color = RGB Int Int Int | HSL Int Int Int

data Storage = HDD String Int Int | SSD String Int
    deriving (Show, Eq)

instance Show Color where
    show (RGB r g b) = "RGB(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"
    show (HSL r g b) = "HSL(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

data Vector3 = V Int Int Int
    deriving (Show, Eq)

componentSum :: Vector3 -> Int
componentSum (V x y z) = x + y + z

crossProduct :: Vector3 -> Vector3 -> Vector3
crossProduct (V a1 a2 a3) (V b1 b2 b3) = V ((a2*b3)-(a3*b2)) ((a3*b1)-(a1*b3)) ((a1*b2)-(a2*b1))

vectorListSum :: [Vector3] -> Vector3
vectorListSum [] = V 0 0 0
vectorListSum xs = (V (suma xs) (sumb xs) (sumc xs))

suma :: [Vector3] -> Int
suma [] = 0
suma ((V a b c):xs) = a + suma xs
sumb :: [Vector3] -> Int
sumb [] = 0
sumb ((V a b c):xs) = b + sumb xs
sumc :: [Vector3] -> Int
sumc [] = 0
sumc ((V a b c):xs) = c + sumc xs

capacity (HDD gyarto rpm gb) = gb
capacity (SSD gyarto gb) = gb

isHDD :: Storage -> Bool
isHDD (HDD g r b) = True
isHDD (SSD g b) = False

hugeHDDs :: [Storage] -> [Storage]
--hugeHDDs [] = []
--hugeHDDs ((SSD a b):xs) = undefined 
--hugeHDDs ((HDD a b c):xs) | c > seged xs = (HDD a b c) : hugeHDDs xs
--                          | otherwise = hugeHDDs xs
hugeHDDs xs = [ (HDD a b c) | (HDD a b c) <- xs, c > (seged xs)]

seged :: [Storage] -> Int
seged [(HDD a b c)] = 0
seged [(SSD a b)] = b
seged ((HDD _ _ _) : xs) = seged xs
seged ((SSD a b) : xs) 
    | b > maxTail = b
    | otherwise = maxTail
    where maxTail = seged xs