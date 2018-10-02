module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

-- Exercise 2: (0:30)

-- This function returns a random list of n numbers with lowerbound a and upperbound b.
randList :: Int -> Int -> Int -> IO [Int]
randList 0 _ _= return []
randList n a b = do
    num <- randomRIO (a,b)
    rest <- randList (n-1) a b
    return (num:rest)

-- This function returns a random set of max n numbers with lowerbound a and upperbound b.
randSet :: Int -> Int -> Int -> IO (Set Int)
randSet n a b = do
    rlist <- randList n a b
    return $ list2set rlist

-- Lukas' functions:
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




-- Exercise 3: (3:00)
setIntersect :: Ord a => Set a -> Set a -> Set a -> Set a
setIntersect (Set []) _ z = z
setIntersect (Set (x:xs)) ys z
    | inSet x ys    = setIntersect (Set xs) ys (insertSet x z)
    | otherwise     = setIntersect (Set xs) ys z

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion x y = unionSet x y

setDifference :: (Ord a) => Set a -> Set a -> Set a
setDifference x y = f y (setUnion x y) where
    f (Set (x:xs)) y = f (Set xs) (deleteSet x y)
    f (Set []) y = y

{--
This function checks whether a property, that requires 1 set as input, is true.
The first parameter is a function of a random set generator.
The second parameter is the function of the property to be tested.
The third parameter is how often you want to test the property.
--}
testCheck1 :: (Int -> Int -> Int -> IO (Set Int)) -> (Set Int -> Bool) -> Int -> IO ()
testCheck1 _ p 0 = putStrLn "Test completed!"
testCheck1 r p n = do
    set <- r 10 1 20
    if (p set) then
        testCheck1 r p (n-1)
    else
        putStrLn "Test failed!"

{--
This function checks whether a property, that requires 2 sets as input, is true.
The first parameter is a function of a random set generator.
The second parameter is the function of the property to be tested.
The third parameter is how often you want to test the property.
--}
testCheck2 :: (Int -> Int -> Int -> IO (Set Int)) -> (Set Int -> Set Int -> Bool) -> Int -> IO ()
testCheck2 _ p 0 = putStrLn "Test completed!"
testCheck2 r p n = do
    set1 <- r 10 1 20
    set2 <- r 10 1 20
    if (p set1 set2) then
        testCheck2 r p (n-1)
    else
        putStrLn "Test failed!"

{--
This function checks whether a property, that requires 3 sets as input, is true.
The first parameter is a function of a random set generator.
The second parameter is the function of the property to be tested.
The third parameter is how often you want to test the property.
--}
testCheck3 :: (Int -> Int -> Int -> IO (Set Int)) -> (Set Int -> Set Int -> Set Int -> Bool) -> Int -> IO ()
testCheck3 _ p 0 = putStrLn "Test completed!"
testCheck3 r p n = do
    set1 <- r 10 1 20
    set2 <- r 10 1 20
    set3 <- r 10 1 20
    if (p set1 set2 set3) then
        testCheck3 r p (n-1)
    else
        putStrLn "Test failed!"


-- A intersect B == B intersect A
prop3a :: Ord (a) => Set a -> Set a -> Bool
prop3a x y = (setIntersect x y (Set [])) == (setIntersect y x (Set []))
{--
testCheck2 getRandomSet prop3a 100
Test completed!

testCheck2 getRandomSetQC prop3a 100
Test completed!
--}

-- A union B == B union A
prop3b :: Ord (a) => Set a -> Set a -> Bool
prop3b x y = (setUnion x y) == (setUnion y x)
{--
testCheck2 getRandomSet prop3b 100
Test completed!

testCheck2 getRandomSetQC prop3b 100
Test completed!
--}

-- A intersect A == A
prop3c :: Ord (a) => Set a -> Bool
prop3c x = setIntersect x x (Set []) == x
{--
testCheck1 getRandomSet prop3c 100
Test completed!

testCheck1 getRandomSetQC prop3c 100
Test completed!
--}

-- A union A == A
prop3d :: Ord (a) => Set a -> Bool
prop3d x = setUnion x x == x
{--
testCheck1 getRandomSet prop3d 100
Test completed!

testCheck1 getRandomSetQC prop3d 100
Test completed!
--}

-- A diff A == {}
prop3e :: Ord (a) => Set a -> Bool
prop3e x = setDifference x x == Set []
{--
testCheck1 getRandomSet prop3e 100
Test completed!

testCheck1 getRandomSetQC prop3e 100
Test completed!
--}

-- (A union B) union C == A union (B union C)
prop3f :: Ord (a) => Set a -> Set a -> Set a -> Bool
prop3f x y z = setUnion (setUnion x y) z == setUnion x (setUnion y z)
{--
testCheck3 getRandomSet prop3f 100
Test completed!

testCheck3 getRandomSetQC prop3f 100
Test completed!
--}

-- (A intersect B) intersect C == A intersect (B intersect C)
prop3g :: Ord (a) => Set a -> Set a -> Set a -> Bool
prop3g x y z = setIntersect (setIntersect x y (Set [])) z (Set []) == setIntersect x (setIntersect y z (Set [])) (Set [])
{--
testCheck3 getRandomSet prop3g 100
Test completed!

testCheck3 getRandomSetQC prop3g 100
Test completed!
--}

prop3h :: Ord (a) => Set a -> Set a -> Set a -> Bool
prop3h x y z = a == b
    where
        a = setUnion x (setIntersect y z (Set []))
        b = setIntersect (setUnion x y) (setUnion x z) (Set [])
{--
testCheck3 getRandomSet prop3h 100
Test completed!

testCheck3 getRandomSetQC prop3h 100
Test completed!
--}


prop3i :: Ord (a) => Set a -> Set a -> Set a -> Bool
prop3i x y z = a == b
    where
        a = setIntersect x (setUnion y z) (Set [])
        b = setUnion (setIntersect x y (Set [])) (setIntersect x z (Set []))
{--
testCheck3 getRandomSet prop3i 100
Test completed!

testCheck3 getRandomSetQC prop3i 100
Test completed!
--}

-- A union (B diff A) == A union B
prop3j :: Ord a => Set a -> Set a -> Bool
prop3j x y = setUnion x (setDifference y x) == setUnion x y
{--
testCheck2 getRandomSet prop3j 100
Test completed!

testCheck2 getRandomSetQC prop3j 100
Test completed!
--}

-- A intersect (B diff A) = {}
prop3k :: Ord a => Set a -> Set a -> Bool
prop3k x y = setIntersect x (setDifference y x) (Set []) == Set []
{--
testCheck2 getRandomSet prop3k 100
Test completed!

testCheck2 getRandomSetQC prop3k 100
Test completed!
--}



-- Exercise 5: (0:20)
type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos x = x `union` [(b,a) | (a,b) <- x]


-- Exercise 6
infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos x = x