module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

-- ex2
-- https://stackoverflow.com/questions/30740366/list-with-random-numbers-in-haskell
randomList :: Int -> IO([Int])
randomList n = sequence $ replicate n $ randomRIO ((-1000),1000::Int)

randomSet :: IO (Set Int)
randomSet = do
  mylength <- randomRIO(0,1000)
  myList <- randomList mylength
  let mySet = list2set myList
  return mySet


randomSetQC :: IO (Set Int)
randomSetQC = do
  myList <- generate (arbitrary :: Gen [Int])
  let mySet = list2set myList
  return mySet

randomSetQC2 :: Gen (Set Int)
randomSetQC2 = do
  myList <- arbitrary
  return (list2set myList)

intersection :: (Ord a) => Set a -> Set a -> Set a
intersection x y = intersection2 x y emptySet where
  intersection2 (Set []) _ z = z
  intersection2 (Set (x:xs)) y z =
                             if inSet x y then intersection2 (Set xs) y (insertSet x z) else intersection2 (Set xs) y z

myUnion :: (Ord a) => Set a -> Set a -> Set a
myUnion x y = unionSet x y

myDifference :: (Ord a) => Set a -> Set a -> Set a
myDifference x y = myDifference2 (myUnion x y) (intersection x y) where
  myDifference2 x (Set []) = x
  myDifference2 x (Set (y:ys)) = if inSet y x then myDifference2 (deleteSet y x) (Set ys) else myDifference2 x (Set ys)




--intersectionIO :: (Ord a) => IO (Set a) -> IO (Set a) -> IO (Set a)
--intersectionIO x y = intersectionIO2 x y emptySet where
--  intersectionIO2 (Set []) _ z = z
--  intersectionIO2 (Set (x:xs)) y z = if isEmpty (Set (x:xs)) then z else
--                             if inSet x y then intersectionIO2 (Set xs) y (insertSet x z) else intersectionIO2 (Set xs) y z


testset1 = list2set [1,2,3,4]
testset2 = list2set [3,4,5,6]

testintersection = do
  myset1 <- randomSet
  myset2 <- randomSet
  return ((intersection myset1 myset2) == list2set[3,4])

-- ex 5
type Rel a = [(a,a)]

--emptyRel :: Rel a
--emptyRel = Rel []

--symClos :: Ord a => Rel a -> Rel a
--symClos x = symClos2 x (Set []) where
--  symClos2 [] y = y
--  symClos2 ([(x1,x2):xs]) y = symClos2 xs ([(x1,x2),(x2,x1):y])

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((x1,x2):xs) = nub((x1,x2):(x2,x1):(symClos(xs)))

--symClos :: Ord a => Rel a -> Rel a
--symClos x = symClos2 x y where
--  symClos2 [] y = y
--  symClos2 ([(x1,x2):xs]) ([(y1,y2):ys]) =


-- ex 6

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w]

-- Transitive closure of relation, sorted and without duplicates
trClos :: Ord a => Rel a -> Rel a
trClos x = sort(nub(until (==(trClos2 . trClos2) x) (trClos2) x)) where
  trClos2 [] = []
  trClos2 (x:xs) = (([x]) @@ xs) ++ (trClos2 xs) ++ (x:xs)


-- ex 7

-- symmetric clsure of symmetric closure is equal to symmetric clusure
propSymEq :: Rel Int -> Bool
propSymEq x = symClos x == symClos(symClos x)
test1 = quickCheck propSymEq

-- length of x after symclos should be larger or equal to length of x,
-- but smaller or equal to two times length of x
propSymLen :: Rel Int -> Bool
propSymLen x = (length (symClos x)) >= (length x) && (length x) <= (length (symClos x))
test2 = quickCheck propSymLen

-- symClos x contains same elements as x. Used nub to remove duplicates.
propSymContainsOldElements :: Rel Int -> Bool
propSymContainsOldElements x = ((nub(x)) \\ (symClos (nub(x)))) == []
test3 = quickCheck propSymContainsOldElements


propTrEq :: Rel Int -> Bool
propTrEq x = trClos x == trClos(trClos x)
test4 = quickCheck propTrEq

propTrLength :: Rel Int -> Bool
propTrLength x = (length (trClos x)) >= (length (nub(x))) && ((length (nub(x))) * (length (nub(x)))) >= (length (trClos x))
test5 = quickCheck propTrLength


propTrContainsOldElements :: Rel Int -> Bool
propTrContainsOldElements x = ((nub(x)) \\ (trClos (nub(x)))) == []
test6 = quickCheck propTrContainsOldElements



-- ex 8

-- Are not the same.
-- Example: [(1, 2), (2, 3), (3, 4)]
-- symClos(trClos [(1,2),(2,3),(3,4)]) ==
-- [(1,2),(2,1),(1,3),(3,1),(1,4),(4,1),(2,3),(3,2),(2,4),(4,2),(3,4),(4,3)]
-- trClos(symClos [(1,2),(2,3),(3,4)]) ==
-- [(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,3),(2,4),(3,1),(3,2),(3,3),(3,4),(4,2),(4,3),(4,4)]
-- Are not equal, second one also contains relations to self (such as (1,1)).

testEqualClosures = (trClos(symClos [(1,2),(2,3),(3,4)]) == symClos(trClos [(1,2),(2,3),(3,4)]))
