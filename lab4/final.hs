module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

{--
Exercise 1 (2 hours)

Question1:
In chapter 4 they talk about sets and how they specify a set.
In this particular example they gave the example of a specification as follows:

{n^2| n ∈ {0, . . . , 999}}

Now my question is does this specify a finite set from {0^2, ..., 999^2}?
Or is it just to check wheter a number n is within a set {0, ..., 999}?


Question2:
Why is a set with a single element not the same as a that single element?


Question3:
Why is a set {a,b,b} the same as {a,b}?

--}

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

-- Exercise 3 (3 hours)
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

s1 = Set [1,2,3,4]
s2 = Set [3,4,5,6]

test3a = setIntersect s1 s2 (Set [])
test3b = do
    s1 <- getRandomSet 10 (-9) 9
    s2 <- getRandomSet 10 (-9) 9
    print s1
    print s2
    return (setIntersect s1 s2 (Set []))

test3c = setUnion s1 s2
test3d = setDifference s1 s2

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

{--
Exercise 4 (2 hours)

Question1:
The following definition was given for the domain of R:
"The set dom (R) = {x | ∃y ( xRy )}, i.e., the set consisting of all first coordinates
of pairs in R, is called the domain of R"

Now they gave the example R = {(1, 4),(1, 5),(2, 5)} which, according to the book, has
the domain "dom (R) = {1, 2}" but why is it not "dom (R) = {1, 1, 2}"?

This might be resulting in the same answer as Question3 from Exercise1

Question2:
So the integer partitions of 4 where 4 ∈ N+ are:

A = [4], [1, 3], [2, 2], [1, 1, 2], [1, 1, 1, 1].

Now they say that |A| == n in this case 4 but this results in 5? So do they skip one partition or something?

--}

{--
Exercise 5
symClos will make a rel with its reversed.
Time spent 30 min.
--}

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos xs = nub $ sort (xs `union` [(y,x) | (x,y) <- xs])

-- Exercise 6 (1 hour)

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w]

-- Transitive closure of relation, sorted and without duplicates
trClos :: Ord a => Rel a -> Rel a
trClos xs 
    | xs /= temp = trClos temp
    | otherwise = xs
    where temp = (xs `union` concat [ concat [ ([a] @@ [b]) | a <- xs, a /= b] | b <- xs])

-- exercise 7 (30 min)

-- symmetric clsure of symmetric closure is equal to symmetric clusure
propSymEq :: Rel Int -> Bool
propSymEq x = symClos x  == symClos(symClos x)
test1 = quickCheck propSymEq

-- length of x after symclos should be larger or equal to length of x,
-- but smaller or equal to two times length of x
propSymLen :: Rel Int -> Bool
propSymLen x = (length (symClos x)) >= (length x) && (length x) <= (length (symClos x))
test2 = quickCheck propSymLen

-- Exercise 8 (5 min)

-- Are not the same.
-- Example: [(1, 2), (2, 3), (3, 4)]
-- symClos(trClos [(1,2),(2,3),(3,4)]) ==
-- [(1,2),(2,1),(1,3),(3,1),(1,4),(4,1),(2,3),(3,2),(2,4),(4,2),(3,4),(4,3)]
-- trClos(symClos [(1,2),(2,3),(3,4)]) ==
-- [(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,3),(2,4),(3,1),(3,2),(3,3),(3,4),(4,2),(4,3),(4,4)]
-- Are not equal, second one also contains relations to self (such as (1,1)).
