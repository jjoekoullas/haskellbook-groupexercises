--1.
module Jammin where

--8.
import Data.List

data Fruit =
    Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Show, Ord)


--2.
data JamJars =
  Jam {
    fruit :: Fruit
  , amount :: Int
  }
  deriving (Eq, Show, Ord)


--3. 4 + 2^64
--4. See above instances.
--5.
row1 = Jam Peach      1
row2 = Jam Plum       2
row3 = Jam Apple      3
row4 = Jam Blackberry 4
row5 = Jam Peach      5
row6 = Jam Plum       6
allJam = [row1, row2, row3, row4, row5, row6]

--6.
totalJars :: Int
totalJars = sum $ amount <$> allJam

--7.
mostRow :: JamJars
mostRow = maximumBy (\x y -> compare (amount x) (amount y)) allJam

--9.
compareKind :: JamJars -> JamJars -> Ordering
compareKind (Jam k _) (Jam k' _) = compare k k'

sortedJam = sortBy compareKind allJam

--10.
groupedJam = groupBy (\(Jam k _) (Jam k' _) -> k == k') sortedJam
