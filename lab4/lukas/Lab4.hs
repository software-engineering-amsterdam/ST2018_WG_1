module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

-- Exercise 1

-- Exercise 2 (1.5 hour)
-- This function creates a random set with lowerbound a,
-- upperbound b and maximum length n.
getRandomSet :: Int -> Int -> Int -> IO (Set Int)
getRandomSet n a b = do
    l <- getRandomList n a b
    let r = list2set l
    return r

-- This function creates a random list using randomRIO
-- with lowerbound a, upperbound b and length n.
getRandomList :: Int -> Int -> Int -> IO ([Int])
getRandomList n a b = sequence (replicate n (randomRIO (a,b::Int)))

-- This function creates a random set using quickcheck
-- with lowerbound a, upperbound b and maximum length n.
getRandomSetQC :: Int -> Int -> Int -> IO (Set Int)
getRandomSetQC n a b = do
    l <- getRandomListQC n a b
    let r = list2set l
    return r

-- This function creates a random list using generate and show
-- from quickcheck with lowerbound a, upperbound b and length n.
getRandomListQC :: Int -> Int -> Int -> IO ([Int])
getRandomListQC n a b = sequence (replicate n (generate (choose (a, b))))

-- Exercise 3 (1.5 hour)
setIntersect :: (Ord a) => Set a -> Set a -> Set a -> Set a
setIntersect (Set []) _ z = z
setIntersect (Set (x:xs)) y z
    | inSet x y = setIntersect (Set xs) y (insertSet x z)
    | otherwise = setIntersect (Set xs) y z

s1 = Set [1,2,3,4]
s2 = Set [3,4,5,6]

test3a = setIntersect s1 s2 (Set [])
test3b = do
    s1 <- getRandomSet 10 (-9) 9
    s2 <- getRandomSet 10 (-9) 9
    print s1
    print s2
    return (setIntersect s1 s2 (Set []))

setUnion :: (Ord a) => Set a -> Set a -> Set a
setUnion x y = unionSet x y

test3c = setUnion s1 s2

setDifference :: (Ord a) => Set a -> Set a -> Set a
setDifference x y = f (setIntersect x y (Set [])) (setUnion x y) where
    f (Set (x:xs)) y = f (Set xs) (deleteSet x y)
    f (Set []) y = y

test3d = setDifference s1 s2

prop3a :: Ord (a) => Set a -> Set a -> Bool
prop3a x y = (setIntersect x y (Set [])) == (setIntersect y x (Set []))

prop3b :: Ord (a) => Set a -> Set a -> Bool
prop3b x y = (setUnion x y) == (setUnion y x)

prop3c :: Ord (a) => Set a -> Set a -> Bool
prop3c x y = (setDifference x y) == (setDifference y x)

prop3d :: Ord (a) => Set a -> Bool
prop3d x = setIntersect x x (Set []) == x

prop3e :: Ord (a) => Set a -> Bool
prop3e x = setUnion x x == x

prop3f :: Ord (a) => Set a -> Bool
prop3f x = setDifference x x == Set []

prop3g :: Ord (a) => Set a -> Set a -> Set a -> Bool
prop3g x y z = setUnion (setUnion x y) z == setUnion (setUnion y z) x

prop3h :: Ord (a) => Set a -> Set a -> Set a -> Bool
prop3h x y z = setDifference (setDifference x y) z ==
                setDifference (setDifference y z) x
