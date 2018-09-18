
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)


-- Exercise 1: (1:00)
-- This function checks how many numbers are in each quartile.
-- USAGE: testProbs <# numbers>
-- OUTPUT: [<1st quartile>, <2nd quartile>, <3th quartile>, <4th quartile>]
-- The output is a list of Ints, where each element is a quartile.
-- These numbers are how many numbers of the so called 'random' function
-- are in each quartile.

testProbs :: Int -> IO [Int]
testProbs n = do
    list <- probs n
    return $ checkCount list
    where
        checkCount l = [first l, second l, third l, fourth l]
        first l = length (filter (\x -> x > 0 && x < 0.25) l)
        second l = length (filter (\x -> x >= 0.25 && x < 0.5) l)
        third l = length (filter (\x -> x >= 0.5 && x < 0.75) l)
        fourth l = length (filter (\x -> x >= 0.75 && x < 1) l)


-- Exercise 2: (2:00)
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | a + b <= c = NoTriangle
    | a + c <= b = NoTriangle
    | b + c <= a = NoTriangle
    | a == b && a == c = Equilateral
    | a == b = Isosceles
    | a == c = Isosceles
    | b == c = Isosceles
    | a^2 + b^2 == c^2 = Rectangular
    | a^2 + c^2 == b^2 = Rectangular
    | b^2 + c^2 == a^2 = Rectangular
    | otherwise = Other

pythTriples :: [(Integer,Integer,Integer)]
pythTriples = filter (\ (x,y,z) -> x^2 + y^2 == z^2) [ (x,y,z) | z <- [1..], x <- [1..z], y <- [1..z], x < y  ]

getPythTriples :: Int -> [(Integer, Integer, Integer)]
getPythTriples n = take n pythTriples

noTriTest = quickCheckResult (\(Positive a) (Positive b) (Positive c) -> triangle a b (a+b+c) == NoTriangle)
equiTriTest = quickCheckResult (\(Positive a) -> triangle a a a == Equilateral)
isoTriTest = quickCheckResult (\(Positive a) -> triangle (2*a) (2*a) (4*a-1) == Isosceles)
rectTriTest = all (\(a,b,c) -> triangle a b c == Rectangular) $ getPythTriples 100


-- Exercise 3a: (1:00)
forall = flip all
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

ex3a, ex3b, ex3c :: Int -> Bool
ex3a n = even n && n > 3
ex3b n = even n || n > 3
ex3c n = (even n && n > 3) || even n

p3a = stronger [(-10)..10] ex3a even
p3b = stronger [(-10)..10] ex3b even
p3c = stronger [(-10)..10] ex3c even
p3d = stronger [(-10)..10] even ex3c

-- Exercise 3b (1:00)
-- Result: [a, c, d, b] (stronger to weaker)

data Function a b = Function {
    name :: String,
    fn :: (Int -> Bool)
}

props = [Function "a" ex3a, Function "b" ex3b, Function "c" ex3c, Function "d" ex3c]


compar :: [a] -> (a -> Bool) -> (a -> Bool) -> String
compar xs p q = let pq = stronger xs p q
                    qp = stronger xs q p
                in
                    if pq && qp then "EQ"
                    else if pq  then "GT"
                    else if qp  then "LT"
                    else             "NO"

order x y
    | stronger list a b = GT
    | stronger list b a = LT
    | otherwise         = EQ
    where
        list = [(-10)..10]
        a = fn x
        b = fn y

ex3 = [name x | x <- reverse (sortBy order props)]

-- Exercise 4: (1:00)

-- This function sorts a list from low to high
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort [a | a <- xs, a <= x] ++ [x] ++ [a | a <- xs, a > x]


isPermutation :: (Eq a, Ord a) => [a] -> [a] -> Bool
isPermutation x y
    | x == y = True
    | otherwise = (quickSort x) == (quickSort y)


isEqualLength :: [a] -> [a] -> Bool
isEqualLength x y = length x == length y

isAssoc :: Ord a => [a] -> [a] -> Bool
isAssoc x y = isPermutation x y == isPermutation y x

isTrans :: Ord a => [a] -> [a] -> [a] -> Bool
isTrans x y z
    | isPermutation x y && isPermutation y z && not (isPermutation x z) = False
    | otherwise = True


-- Exercise 5: (1:00)
isDerangement :: (Eq a, Ord a) => [a] -> [a] -> Bool
isDerangement x y = isPermutation x y && all (==True) (map (\(x,y) -> x /= y) (zip x y))

deran :: Int -> [[Int]]
deran n = [list] ++ [x | x <- permutations list, isDerangement x list]
    where
        list = [0..(n-1)]

deranLength :: Ord a => [a] -> [a] -> Bool
deranLength x y
    | length x == length y  = True
    | otherwise             = False


-- Exercise 6: (1:00)
rot13 :: [Char] -> [Char] -> Bool
rot13 [] y = y
rot13 (x:xs) y = rot13 xs (y ++ )