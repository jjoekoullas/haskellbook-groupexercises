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

main :: IO ()
main = do
  putStrLn "hello world"
