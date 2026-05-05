--PN8G68
module Hazi11 where
import Data.Char

cipher :: String -> String
cipher [] = []
cipher [x] = []
cipher [x,y] = []
cipher (x:xs) | ((toLower x) `elem` "abcdefghijklmnopqrstuvwxyz") && ((toLower (head xs)) `elem` "abcdefghijklmnopqrstuvwxyz") && ((toLower (head (tail xs))) `elem` "1234567890") = [x] ++ [head xs]
              | otherwise = cipher xs

type Username = String
type Password = String 

data Privilege = Simple | Admin
    deriving (Eq, Show)

data Cookie = LoggedOut | LoggedIn Username Privilege
    deriving (Eq, Show)

data Entry = Entry Password Privilege [Username]
    deriving (Eq, Show)

type Database = [(Username, Entry)]

richard, charlie, carol, david, kate :: (Username, Entry)
richard = ("Richard", Entry "password1" Admin  ["Kate"])
charlie = ("Charlie", Entry "password2" Simple ["Carol"])
carol   = ("Carol",   Entry "password3" Simple ["David", "Charlie"])
david   = ("David",   Entry "password4" Simple ["Carol"])
kate    = ("Kate",    Entry "password5" Simple ["Richard"])

testDB :: Database
testDB = [ richard, charlie, carol, david, kate ]

testDBWithoutCarol :: Database
testDBWithoutCarol =
  [ ("Richard", Entry "password1" Admin  ["Kate"])
  , ("Charlie", Entry "password2" Simple [])
  , ("David",   Entry "password4" Simple [])
  , ("Kate",    Entry "password5" Simple ["Richard"])
  ]

password :: Entry -> Password
password (Entry paassword privilege xs) = paassword 

privilege :: Entry -> Privilege
privilege (Entry paassword privilege xs) = privilege 

friends :: Entry -> [Username]
friends (Entry a b xs)  = xs

mkCookie :: Username -> Password -> Entry -> Cookie
mkCookie username password (Entry password' priv xs) | password == password' = LoggedIn username priv
                                                     | otherwise = LoggedOut

login :: Username -> Password -> Database -> Cookie
login un pw [(un', Entry pw' priv xs)] | un == un' && pw == pw' = LoggedIn un priv
                                    | otherwise = LoggedOut
login un pw ((un', Entry pw' priv xs):ys) | un == un' && pw == pw' = LoggedIn un priv
                                         | otherwise = login un pw ys