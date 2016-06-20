
{-# LANGUAGE FlexibleInstances #-}

module Main where

data PugType = PugData
data HuskyType a = HuskyData
data DogueDeBordeaux doge = DogueDeBordeaux doge

data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)

--Exercises: Dog Types
--1. Type constructor (p 414)
--2. * -> * (:k Doggies)
--3. * (:k Doggies String)
--4. Num a => Doggies a (:t Husky 10)
--5. Doggies Integer (:t Husky (10 :: Integer))
--6. Doggies [Char] (:t Mastiff "Scooby Doo")
--7. Both.  On the left side of =, type constructor, on right, data constructor
--8. doge -> DogueDeBordeaux doge (:t DogueDeBordeaux)
--9. DogueDeBordeaux [Char] (:t DogueDeBordeaux "doggie!")



data Price =
  Price Integer deriving (Eq, Show)

data Manufacturer =
    Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline =
    PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline
             deriving (Eq, Show)

myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuAir

--Exercises: Vehicles
--1. Vehicle (:t myCar)
--2.
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _         = False

areCars :: [Vehicle] -> [Bool]
areCars = fmap isCar
--some alternatives:
--areCars vs = isCar <$> vs
--areCars vs = vs `map` isCar

--3.
getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
--getManu _         = error "Don't know how to get a manufacturer of anything not a car"
--4. An error. Without the explicit error instead an error about a non-exhaustive
--  pattern is recieved. Turning on -Wall will cause these non-exhaustive patterns to
--  produce warnings on compilation.
--5.
newtype Size = Size Int deriving (Eq, Show)
data Vehicle' = Car' Manufacturer Price
              | Plane' Airline Size
              deriving (Eq, Show)

isPlane' :: Vehicle' -> Bool
isPlane' (Plane' _ _) = True
isPlane' _            = False



--Exercises: Cardinality
--1. 1
--2. 3
--3. 2 ^ 16 ((fromIntegral (maxBound :: Int16)) - (fromIntegral (minBound :: Int16)) + 1)
--  had to import Data.Int for Int16
--4. Int is a 64 bit integer (2^64 inhabitants), while Integer is unbounded (infinite inhabitants)
--5. Number of bits.  My friends weren't impressed. :(


data Example = MakeExample deriving Show

--Exercises: For Example
--1. Example (:t MakeExample)
--   :t Example complains that Example is not a data constructor
--2. Show (:i Example)
--3. a -> MyExample a (:t MakeMyExample)
data MyExample a = MakeMyExample a deriving Show


--Exercises: Logic Goats
--1.
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (n, _) = tooMany n 

--2.
instance TooMany (Int, Int) where
  tooMany (x, y) = tooMany $ x + y



--Exercises: Pity the Bool
--1. 2 + 2 = 4
--2. 256 + 2 = 258



--Exercises: How Does Your Garden Grow?
--1.
data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show
type Gardener = String
data Garden =
  Garden Gardener FlowerType
  deriving Show

data NFGarden =
    NFGardenia Gardener
  | NFDaisy    Gardener
  | NFRose     Gardener
  | NFLilac    Gardener
  deriving Show


--Exercise: Programmers
data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os   :: OperatingSystem
             , lang :: ProgrammingLanguage }
  deriving (Eq,Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer os' lang' | os'   <- allOperatingSystems,
                                          lang' <- allLanguages ] 


--exponentiation
{-
data Quantum =
    Yes
  | No
  | Both
  deriving (Eq, Show)

convert :: Quantum -> Bool

1 = Yes|No|Both -> True
2 = Yes|No|Both -> False
3 = Yes|No -> True
    Both   -> False
4 = Yes|No -> False
    Both   -> True
5 = Yes|Both -> True
    No       -> False
6 = Yes|Both -> False
    No       -> True
7 = No|Both -> True
    Yes     -> False
8 = No|Both -> False
    Yes     -> True
-}

--Exercise: The Quad
--1. 4 + 4 = 16
data Quad =
    One
  | Two
  | Three
  | Four
  deriving (Eq, Show)
--2. 4 * 4 = 16
prodQuadCount :: Int
prodQuadCount = length [(a,b) | a <- [One, Two, Three, Four], b <- [One, Two, Three, Four]]
--3. 4 ^ 4 = 256
--4. 2 * 2 * 2 = 8
--5. 2 ^ 2 ^ 2 = 16
--6. (2 ^ 4) ^ 4 = 65536.  2^4 = Bool -> Quad, ^ 4 = -> Quad.
--  Need parenthesis to flip associativity around, showing -> associates ltr, ^ rtl



data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right 

--  2
--1   3
testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2,1,3]
  then putStrLn "Preorder fine!"
  else putStrLn "Preorder bad!"

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

testInorder :: IO ()
testInorder =
  if inorder testTree == [1,2,3]
  then putStrLn "Inorder fine!"
  else putStrLn "Inorder bad!"

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1,3,2]
  then putStrLn "Postorder fine!"
  else putStrLn "Postorder bad!"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) =
  let leftFolded = foldTree f b left
      folded     = f a leftFolded
  in foldTree f folded right

testFoldTree :: IO ()
testFoldTree =
  if inorder testTree == (reverse $ foldTree (:) [] testTree)
  then putStrLn "foldTree fine!"
  else putStrLn "foldTree bad!"