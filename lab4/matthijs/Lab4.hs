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
  return (intersection myset1 myset2)
