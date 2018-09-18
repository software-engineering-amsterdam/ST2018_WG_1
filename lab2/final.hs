
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import System.Process

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
forall :: [a] -> (a->Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

--Properties from workshop 2
f1, f2, f3, f4 :: Int -> Bool
f1 = (\ x -> (even x && x > 3))
f2 = (\ x -> even x || x > 3)
f3 = (\ x -> (even x && x > 3) || even x)
f4 = (\ x -> even x)

mytest = filter (f1) [-10..10]
mytest2 = filter (f2) [-10..10]

mystronger myf1 myf2 = stronger [-10..10] myf1 myf2

funclist = [f1,f2,f3,f4]
funcliststring = ["f1", "f2", "f3", "f4"]


-- Prints the functions ordered by strength (f1, f4, f3, f2)
descliststrength :: IO()
descliststrength = descliststrength2 funclist [] [] funcliststring [] []

descliststrength2 :: [(Int -> Bool)] -> [(Int -> Bool)] -> [(Int -> Bool)] -> [String] -> [String] -> [String] -> IO()
descliststrength2 (x:y:funclist) m n (xs:ys:funcliststring) ms ns
  | mystronger x y = descliststrength2 (y:funclist) (x:m) n (ys:funcliststring) (xs:ms) ns
  | mystronger y x = descliststrength2 (x:funclist) (y:m) n (xs:funcliststring) (ys:ms) ns
descliststrength2 [x] m n [xs] ms ns = descliststrength2 m [] (x:n) ms [] (xs:ns)
descliststrength2 [] _ n _ _ ns = print ns

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

prop4a :: [Int] -> Bool
prop4a x = isPermutation x x == True

prop4b :: [Int] -> [Int] -> Bool
prop4b x y
    | isPermutation x y = (length x == length y)
    | otherwise = True

prop4c :: [Int] -> [Int] -> Bool
prop4c x y = isPermutation x y == isPermutation y x

prop4d :: [Int] -> [Int] -> [Int] -> Bool
prop4d x y z
    | isPermutation x y && isPermutation y z && not (isPermutation x z) = False
    | otherwise = True

prop4e :: [Int] -> [Int] -> Bool
prop4e x y = isPermutation x y == isPermutation y x

test4a = do quickCheck prop4a
test4b = do quickCheck prop4b
test4c = do quickCheck prop4c
test4d = do quickCheck prop4d
test4e = do quickCheck prop4e

-- Recognizing and generating derangements (1 hour)
-- Derangement of empty list does not exist.
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] _ = False
isDerangement _ [] = False
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

prop5a :: [Int] -> Bool
prop5a x = isDerangement x x == False

prop5b :: [Int] -> [Int] -> Bool
prop5b x y
    | isDerangement x y = (length x == length y)
    | otherwise = True

prop5c :: [Int] -> [Int] -> Bool
prop5c x y = isDerangement x y == isDerangement y x

prop5d :: [Int] -> [Int] -> [Int] -> Bool
prop5d x y z
    | isDerangement x y && isDerangement y z && not (isDerangement x z) = False
    | otherwise = True

prop5e :: [Int] -> [Int] -> Bool
prop5e x y = isDerangement x y == isDerangement y x

test5a = do quickCheck prop5a
test5b = do quickCheck prop5b
test5c = do quickCheck prop5c
test5d = do quickCheck prop5d
test5e = do quickCheck prop5e

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
