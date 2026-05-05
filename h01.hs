import Data.Char

intExpr1 :: Int
intExpr1 = 1

intExpr2 :: Int
intExpr2 = intExpr1 + 1

intExpr3 :: Int
intExpr3 = intExpr2 * intExpr2

charExpr1 :: Char
charExpr1 = 'a'

charExpr2 :: Char
charExpr2 = toUpper charExpr1

charExpr3 :: Char
charExpr3 = toLower 'A'

boolExpr1 :: Bool
boolExpr1 = 1 == 2

boolExpr2 :: Bool
boolExpr2 = toLower 'a' == 'a'

boolExpr3 :: Bool
boolExpr3 = False

canPlantAll :: Bool
canPlantAll = mod 183 13 == 0

remainingSeeds :: Int
remainingSeeds = mod 183 13

inc :: Int -> Int
inc a = a + 1

double :: Int -> Int 
double a = 2*a

seven1 :: Int
seven1 = inc (inc (inc (inc (inc (inc (inc 0))))))

seven2 :: Int
seven2 = inc(inc (inc (double (double (inc 0)))))

seven3 :: Int 
seven3 = inc (inc (inc (inc (inc (inc (inc (double 0)))))))

cmpRem5Rem7 :: Int -> Bool
cmpRem5Rem7 a = mod a 5 > mod a 7