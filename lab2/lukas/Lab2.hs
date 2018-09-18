-- Name: lukas Koedijk
-- Student ID: 10783687

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

-- Red Curry (2.5 hour)
-- This function calculates how many ocurences there are in the ranges
-- (0..0.25),[0.25..0.5),[0.5..0.75),[0.75..1)
probTest :: IO [Float] -> IO [Int]
probTest numbers = do
    list <- numbers
    let r1 = filter (\x -> x > 0 && x < 0.25) list
    let r2 = filter (\x -> x >= 0.25 && x < 0.5) list
    let r3 = filter (\x -> x >= 0.5 && x < 0.75) list
    let r4 = filter (\x -> x >= 0.75 && x < 1) list
    return $ [length r1, length r2, length r3, length r4]

-- This test if probs is random. When looking at the outcome we see
-- that most of the time the quartes do not have a difference than
-- a 100 from the 2500 the all should have. This makes probs a decent
-- random float generator.
test1 = probTest (probs 10000)

-- Recognizing triangles (1 hour)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | a + b <= c || b + c <= a || c + a <= b = NoTriangle
    | a == b && a == c = Equilateral
    | (a^2) + (b^2) == (c^2) = Rectangular
    | a == b || b == c || c == a = Isosceles
    | otherwise = Other

-- From first weeks slides
-- To calculate python triples to test Rectangular triangles
pythTriples :: [(Integer,Integer,Integer)]
pythTriples = filter (\ (x,y,z) -> x^2 + y^2 == z^2)
                    [ (x,y,z) | z <- [1..], x <- [1..z], y <- [1..z], x < y]

test2a = quickCheckResult (\ (Positive a) (Positive b) (Positive c) ->
                            triangle a b (a+b+c) == NoTriangle)
test2b = quickCheckResult (\ (Positive a) -> triangle a a a == Equilateral)
test2c = all (\(x,y,z) -> triangle x y z == Rectangular) (take 100 pythTriples)
test2d = quickCheckResult (\ (Positive a) -> triangle (2*a) (2*a) ((4*a)-1) == Isosceles)

-- Testing properties strength (30 min)
forall = flip all
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

p1, p2, p3 :: Int -> Bool
p1 x = even x && x > 3
p2 x = even x || x > 3
p3 x = p1 x || even x

testP :: (Int -> Bool) -> [(Int -> Bool)] -> Integer -> Integer
testP p [] c = c
testP p (x:xs) c
    | stronger [-10..10] p x = testP p xs (c+1)
    | otherwise = testP p xs c

test3 = [testP p1 [p2, p3, even] 0, testP p2 [p1, p3, even] 0,
        testP p3 [p1, p2, even] 0, testP even [p1, p2, p3] 0]

-- Recognizing permutations (1 hour)
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation x y
    | length x == length y = isPermutation2 x y []
    | otherwise = False

isPermutation2 :: Eq a => [a] -> [a] -> [a] -> Bool
isPermutation2 [] [] [] = True
isPermutation2 _ [] _ = False
isPermutation2 (x:xs) (y:ys) n
    | x == y = isPermutation2 xs (ys ++ n) []
    | otherwise = isPermutation2 (x:xs) ys (y:n)

--test4a = quickCheckResult (\ (NonEmpty x) -> isPermutation x x == True)
--test4b = quickCheckResult (\ x y -> isPermutation x (x ++ y) == False)

-- Recognizing and generating derangements (1 hour)
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement x y
    | isPermutation x y = isDerangement2 x y
    | otherwise = False

isDerangement2 :: Eq a => [a] -> [a] -> Bool
isDerangement2 [] [] = True
isDerangement2 (x:xs) (y:ys)
    | x == y = False
    | otherwise = isDerangement2 xs ys

deran :: Int -> [[Int]]
deran n = filter (\x -> isDerangement x [0..(n-1)]) (permutations [0..(n-1)])

--Can use to high number
--test5 = quickCheckResult (\ (Positive x) -> x == 1 || isDerangement [0..x-1] (head (deran x)) == True)

-- Implementing and testing ROT13 (1 hour)
add13 :: Int -> Int
add13 x
    | (x + 13) > (97 + 25) = x + 13 - 26
    | x < 97 && (x + 13) > (65 + 25) = x + 13 - 26
    | otherwise = x + 13

rot13 :: [Char] -> [Char] -> [Char]
rot13 [] y = y
rot13 (x:xs) y = rot13 xs (y ++ [chr (add13 (ord x))])

-- Implementing and testing IBAN validation (30 min)
iban :: String -> Bool
iban s
    | length s > 34 && length s < 4 = False
    | mod (read (iban2 (snd (splitAt 4 (s ++ (take 4 s)))) [])) 97 == 1 = True
    | otherwise = False

iban2 :: [Char] -> [Char] -> [Char]
iban2 [] n = n
iban2 (s:ss) n
    | s >= 'A' && s <= 'Z' = iban2 ss (n ++ (show ((ord s) - 55)))
    | otherwise = iban2 ss (n ++ [s])

test7 = iban "GB82WEST12345698765432"






