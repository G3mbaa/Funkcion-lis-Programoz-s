--PN8G68

isSmallPrime :: Int -> Bool
isSmallPrime 5 = True
isSmallPrime 7 = True
isSmallPrime 11 = True
isSmallPrime 13 = True
isSmallPrime _ = False

equivalent :: Bool -> Bool -> Bool
equivalent True True = True 
equivalent False False = True 
equivalent _ _ = False 

implies :: Bool -> Bool -> Bool
implies True False = False 
implies _ _ = True 

invertX :: (Int, Int) -> (Int, Int)
invertX (x,y) = (x,(-1*y))

xDistance :: (Int, Int) -> (Int, Int) -> Int
xDistance (x,y) (x1,y1) = abs(x - x1)

add :: (Int, Int) -> (Int, Int ) -> (Int, Int)
add (a,b) (c,d) = (((a*d) + (b*c)),b*d)

multiply :: (Int, Int) -> (Int, Int) -> (Int, Int)
multiply (a,b) (c,d) = ((a*c),(b*d))

divide :: (Int, Int) -> (Int, Int) -> (Int, Int)
divide (a,b) (c,d) = ((a*d),(b*c))