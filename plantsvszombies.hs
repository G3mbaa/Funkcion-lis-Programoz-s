--PN8G68 - nagybeadandó
import Data.Maybe
import Data.List

type Coordinate = (Int, Int)
type Sun = Int

data Plant = Peashooter Int | Sunflower Int | Walnut Int | CherryBomb Int
    deriving(Eq, Show)

data Zombie = Basic Int Int | Conehead Int Int | Buckethead Int Int | Vaulting Int Int
    deriving(Eq, Show)

data GameModel = GameModel Sun [(Coordinate, Plant)] [(Coordinate, Zombie)]
    deriving(Eq, Show)

defaultPeashooter :: Plant
defaultPeashooter = Peashooter 3

defaultSunflower :: Plant
defaultSunflower = Sunflower 2

defaultWalnut :: Plant
defaultWalnut = Walnut 15

defaultCherryBomb :: Plant
defaultCherryBomb = CherryBomb 2

basic :: Zombie
basic = Basic 5 1

coneHead :: Zombie
coneHead = Conehead 10 1

bucketHead :: Zombie
bucketHead = Buckethead 20 1

vaulting :: Zombie
vaulting = Vaulting 7 2

bennevan :: Coordinate -> Bool
bennevan (x,y) | (x >= 0 && x < 5) && (y >= 0 && y < 12) = True
               | otherwise = False

tryPurchase :: GameModel -> Coordinate -> Plant -> Maybe GameModel
tryPurchase (GameModel s novenylista zombie) kordi noveny | (bennevan kordi == True) && (lookup kordi novenylista == Nothing) && (noveny == defaultWalnut) && (s-50>=0) = Just(GameModel (s-50) ((kordi, noveny):novenylista) zombie)
                                                            | (bennevan kordi == True) && (lookup kordi novenylista == Nothing) && (noveny == defaultSunflower) && (s-50>=0) = Just(GameModel (s-50) ((kordi, noveny):novenylista) zombie)
                                                            | (bennevan kordi == True) && (lookup kordi novenylista == Nothing) && (noveny == defaultCherryBomb) && (s-150>=0) = Just(GameModel (s-150) ((kordi, noveny):novenylista) zombie)
                                                            | (bennevan kordi == True) && (lookup kordi novenylista == Nothing) && (noveny == defaultPeashooter) && (s-100>=0) = Just(GameModel (s-100) ((kordi, noveny):novenylista) zombie)
                                                            | otherwise = Nothing

placeZombieInLane :: GameModel -> Zombie -> Int -> Maybe GameModel
placeZombieInLane (GameModel s nl zl) zt sav | (sav >= 0 && sav < 5) && (lookup (sav,11) zl == Nothing) = Just (GameModel s nl (((sav,11), zt):zl))
                                             | otherwise = Nothing

performZombieActions :: GameModel -> Maybe GameModel
performZombieActions (GameModel s [] []) = Just (GameModel s [] [] )
performZombieActions (GameModel s ps []) = Just (GameModel s ps [])
performZombieActions (GameModel s ps zs) | zeroZombie (GameModel s ps zs) == Nothing = Nothing
                                         | otherwise = Just (GameModel s (zombieAttacks (GameModel s ps zs)) (zombieMoves (GameModel s ps zs)))

zombieAttack :: (Coordinate, Plant) -> Int -> (Coordinate, Plant)
zombieAttack ((x,y), (Peashooter hp)) a = ((x,y), (Peashooter (hp-a)))
zombieAttack ((x,y), Sunflower hp)    a = ((x,y), (Sunflower (hp-a)))
zombieAttack ((x,y), Walnut hp)       a = ((x,y), (Walnut (hp-a)))
zombieAttack ((x,y), CherryBomb hp)   a = ((x,y), (CherryBomb (hp-a)))

--zombieJump :: Zombie -> Zombie
--zombieJump (Vaulting a b)   = Vaulting a (b-1)
--zombieJump (Buckethead a b) = Buckethead a b
--zombieJump (Conehead a b)   = Conehead a b
--zombieJump (Basic a b)      = Basic a b

zombieSpeed :: Zombie -> Int
zombieSpeed (Basic a b) = b
zombieSpeed (Conehead a b) = b
zombieSpeed (Buckethead a b) = b

zombieCoord :: (Coordinate, Zombie) -> Coordinate
zombieCoord (coord, zombie) = coord

zombieCoord2 :: (Coordinate, Zombie) -> Coordinate
zombieCoord2 ((x,y), zombie) = (x,(y-1))

plantCoord :: (Coordinate, Plant) -> Coordinate
plantCoord (coord, plant) = coord

zombieMove :: (Coordinate, Zombie) -> (Coordinate, Zombie)
zombieMove ((x,y), zombie) = ((x,y-(zombieSpeed zombie)), zombie)

zeroZombie :: GameModel -> Maybe GameModel
zeroZombie (GameModel sun pl [(coord, zombie)]) | (coord == (0,0)) = Nothing
                                                | otherwise = Just (GameModel sun pl [(coord, zombie)])
zeroZombie (GameModel sun plant (zombie:zl))    | (zombieCoord zombie == (0,0)) = Nothing
                                                | otherwise = (zeroZombie (GameModel sun plant zl))

vaultZombie :: (Coordinate, Zombie) -> (Coordinate, Zombie)
vaultZombie ((x,y), Vaulting a 2) = ((x,(y-1)), Vaulting a 1)

vaultZombie2 :: (Coordinate, Zombie) -> (Coordinate, Zombie)
vaultZombie2 ((x,y), Vaulting a b) = ((x,(y-2)), Vaulting a 1)

nagyotUgroZombi :: (Coordinate, Zombie) -> (Coordinate, Zombie)
nagyotUgroZombi ((x,y), Vaulting a b) = ((x,(y-2)), Vaulting a 2)

zombieMoves :: GameModel -> [(Coordinate, Zombie)]
zombieMoves (GameModel s [] [zombie]) = [zombieMove zombie]
zombieMoves (GameModel s [] (zombie:zl)) = (zombieMove zombie) : zombieMoves (GameModel s [] zl)
zombieMoves (GameModel s pl []) = [] 
zombieMoves (GameModel s pl (zombie:zl)) 
    | (isVaulting' zombie == True) && (lookup (zombieCoord zombie) pl /= Nothing) = (vaultZombie zombie) : zombieMoves (GameModel s pl zl)
    | (isVaulting' zombie == True) && (lookup (zombieCoord zombie) pl == Nothing) && (lookup (zombieCoord2 zombie) pl /= Nothing) = (vaultZombie2 zombie) : zombieMoves (GameModel s pl zl)
    | (isVaulting' zombie == True) && (lookup (zombieCoord zombie) pl == Nothing) && (lookup (zombieCoord2 zombie) pl == Nothing) = (nagyotUgroZombi zombie) : zombieMoves (GameModel s pl zl)
    | (lookup (zombieCoord zombie) pl == Nothing) = (zombieMove zombie) : zombieMoves (GameModel s pl zl)
    | otherwise = zombie : zombieMoves (GameModel s pl zl)

zombieAttacks :: GameModel -> [(Coordinate, Plant)]
zombieAttacks (GameModel s pl []) = pl
zombieAttacks (GameModel s [] _) = []
zombieAttacks (GameModel s [p] zl) | (plantCoordCount p zl == 0) = [p]
                                   | otherwise = [zombieAttack p (plantCoordCount p zl)]
zombieAttacks (GameModel s (plant:pl) zl) | ((plantCoordCount plant zl) == 0) = plant : zombieAttacks (GameModel s pl zl)
                                          | otherwise = (zombieAttack plant (plantCoordCount plant zl)) : zombieAttacks (GameModel s pl zl)

isVaulting :: Maybe Zombie -> Bool
isVaulting (Just (Vaulting _ _ )) = True
isVaulting (Just (Conehead _ _)) = False
isVaulting (Just (Basic _ _)) = False
isVaulting (Just (Buckethead _ _)) = False
isVaulting Nothing = False

isVaulting' :: (Coordinate, Zombie) -> Bool
isVaulting' (_, Vaulting _ _) = True
isVaulting' (_, Buckethead _ _) = False
isVaulting' (_, Conehead _ _) = False
isVaulting' (_, Basic _ _) = False

cleanBoard :: GameModel -> GameModel
cleanBoard (GameModel s pl zl) = (GameModel s (killedPlants (GameModel s pl zl)) (killedZombies (GameModel s pl zl)))

killedPlant :: (Coordinate, Plant) -> Bool
killedPlant ((x,y), Peashooter hp) | hp <= 0 = True
                                   | otherwise = False
killedPlant ((x,y), Walnut hp)     | hp <= 0 = True
                                   | otherwise = False
killedPlant ((x,y), CherryBomb hp) | hp <= 0 = True
                                   | otherwise = False
killedPlant ((x,y), Sunflower hp)  | hp <= 0 = True
                                   | otherwise = False



killedPlants :: GameModel -> [(Coordinate, Plant)]
killedPlants (GameModel s [] zl) = []
killedPlants (GameModel s (p:pl) zl) | (killedPlant p == True) = killedPlants (GameModel s pl zl)
                                     | otherwise = p : killedPlants (GameModel s pl zl)

killedZombie :: (Coordinate, Zombie) -> Bool
killedZombie ((x,y), Conehead hp speed) | hp <= 0 = True
                                        | otherwise = False
killedZombie ((x,y), Basic hp speed) | hp <= 0 = True
                                        | otherwise = False
killedZombie ((x,y), Buckethead hp speed) | hp <= 0 = True
                                        | otherwise = False
killedZombie ((x,y), Vaulting hp speed) | hp <= 0 = True
                                        | otherwise = False

killedZombies :: GameModel -> [(Coordinate, Zombie)]
killedZombies (GameModel s pl []) = []
killedZombies (GameModel s pl (z:zl)) | (killedZombie z == True) = killedZombies (GameModel s pl zl)
                                      | otherwise = z : killedZombies (GameModel s pl zl)

plantCoordCount :: (Coordinate, Plant) -> [(Coordinate, Zombie)] -> Int
plantCoordCount _ [] = 0
plantCoordCount p (z:zl) | (plantCoord p) == (zombieCoord z) && (isVaulting' z == False) = 1 + plantCoordCount p zl
                         | otherwise = plantCoordCount p zl