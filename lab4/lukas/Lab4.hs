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

-- Exercise 3 (30 min)
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
