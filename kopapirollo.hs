--PN8G68 - pótlás - 10. házi
module RPS where

type Symbol = Int
type Outcome = Int

isValidSymbol :: Symbol -> Bool
isValidSymbol 0 = True
isValidSymbol 1 = True
isValidSymbol 2 = True
isValidSymbol _ = False

evalTurn :: Symbol -> Symbol -> Outcome
evalTurn 0 2 = -1
evalTurn 2 0 = 1
evalTurn 1 2 = 1
evalTurn 2 1 = -1
evalTurn 1 0 = -1
evalTurn 0 1 = 1
evalTurn a b | a == b = 0

getWinner :: Outcome -> String -> String -> String
getWinner oc a b | oc < 0 = a
                 | oc > 0 = b
                 | oc == 0 = "DONTETLEN"
                
isValidMatch :: ([Symbol], [Symbol]) -> Bool
isValidMatch ([_],[]) = False
isValidMatch ([],[_]) = False
isValidMatch ([],xs) = False
isValidMatch (xs,[]) = False
isValidMatch ([x],[y]) | x >= 0 && x <= 2 && y >= 0 && y <= 2 = True
                       | otherwise = False
isValidMatch ((x:xs),(y:ys)) | isValidMatch ([x],[y]) == True = isValidMatch (xs,ys)
                             | otherwise = False

evalMatch :: ([Symbol],[Symbol]) -> Outcome
evalMatch (xs,ys) = match (xs,ys) 0

match :: ([Symbol],[Symbol]) -> Int -> Outcome
match ([x],[y]) a | evalTurn x y == -1 = (a - 1)
                  | evalTurn x y == 0 = a
                  | otherwise = (a + 1)
match ((x:xs), (y:ys)) a | evalTurn x y == -1 = match (xs,ys) (a - 1)  
                         | evalTurn x y == 0 = match (xs,ys) a  
                         | otherwise = match (xs,ys) (a+1)

rps :: String -> String -> ([Symbol],[Symbol]) -> String
rps a b (xs,ys) | evalMatch (xs,ys) == 0 = "DONTETLEN"
                | evalMatch (xs,ys) < 0 = a
                | otherwise = b