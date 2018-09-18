
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
-- (0..0.25),[0.25..0.5),[0.5..0.75),[0.75..1).
probTest :: IO [Float] -> IO [Int]
probTest numbers = do
    list <- numbers
    let r1 = filter (\x -> x > 0 && x < 0.25) list
    let r2 = filter (\x -> x >= 0.25 && x < 0.5) list
    let r3 = filter (\x -> x >= 0.5 && x < 0.75) list
    let r4 = filter (\x -> x >= 0.75 && x < 1) list
    return $ [length r1, length r2, length r3, length r4]

-- This tests if probs is random. When looking at the outcome we see
-- that most of the time the quartes do not have a difference greater than
-- a 100 from the 2500 they all should have. This makes probs a decent
-- random float generator.
test1 = probTest (probs 10000)

-- Recognizing triangles (1 hour)
-- This function checks if with the length of the sides given a triangle
-- can be formed. If a triangle can be formed the functions checks which
-- type of triange it is.
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | a + b <= c || b + c <= a || c + a <= b = NoTriangle
    | a == b && a == c = Equilateral
    | (a^2) + (b^2) == (c^2) = Rectangular
    | a == b || b == c || c == a = Isosceles
    | otherwise = Other

-- From first weeks slides
-- To calculate python triples to test Rectangular triangles.
pythTriples :: [(Integer,Integer,Integer)]
pythTriples = filter (\ (x,y,z) -> x^2 + y^2 == z^2)
                    [ (x,y,z) | z <- [1..], x <- [1..z], y <- [1..z], x < y]

-- Tests if the function triangle works correctly.
-- When a side is longer than two of the other side combined then
-- a triangle can't be formed.
test2a = quickCheckResult (\ (Positive a) (Positive b) (Positive c) ->
                            triangle a b (a+b+c) == NoTriangle)
-- When all sides of a triangle have the same length,
-- then the triangle is equilateral
test2b = quickCheckResult (\ (Positive a) -> triangle a a a == Equilateral)
-- When the sides of a triangle are a pythagoras triple,
-- then the triangle is rectangular
test2c = all (\(x,y,z) -> triangle x y z == Rectangular) (take 100 pythTriples)
-- When two sides of the triangle are equal,
-- then the triagnle is isosceles
test2d = quickCheckResult (\ (Positive a) -> triangle (2*a) (2*a) ((4*a)-1) == Isosceles)

-- Testing properties strength (1 hour)
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
-- The function isPermutation checks if the length of two list are equal
-- and then checks if the second list is a permutation of the first
-- by calling isPermutation2.
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation x y
    | length x == length y = isPermutation2 x y []
    | otherwise = False

-- This function checks if a list is a permutation of another by looping
-- through the second list to find the first caracter of the first list.
isPermutation2 :: Eq a => [a] -> [a] -> [a] -> Bool
isPermutation2 [] [] [] = True
isPermutation2 _ [] _ = False
isPermutation2 (x:xs) (y:ys) n
    | x == y = isPermutation2 xs (ys ++ n) []
    | otherwise = isPermutation2 (x:xs) ys (y:n)

-- Permutations have the property that a list is a permutation of its self.
prop4a :: [Int] -> Bool
prop4a x = isPermutation x x == True

-- Permutations have the property that the length of two list that are
-- permutations of each other is the same
prop4b :: [Int] -> [Int] -> Bool
prop4b x y
    | isPermutation x y = (length x == length y)
    | otherwise = True

-- Permutations have the property that if x is a permutation of y,
-- y is a permutation of x.
prop4c :: [Int] -> [Int] -> Bool
prop4c x y = isPermutation x y == isPermutation y x

-- Permutations have the property that if x is a permutation of y and
-- y is a permutation of z, then x is a permutation of z.
prop4d :: [Int] -> [Int] -> [Int] -> Bool
prop4d x y z
    | isPermutation x y && isPermutation y z && not (isPermutation x z) = False
    | otherwise = True

-- These are quickCheck tests to test if our permutations function has these
-- properties.
test4a = do quickCheck prop4a
-- +++ OK, passed 100 tests.
test4b = do quickCheck prop4b
-- +++ OK, passed 100 tests.
test4c = do quickCheck prop4c
-- +++ OK, passed 100 tests.
test4d = do quickCheck prop4d
-- +++ OK, passed 100 tests.

-- Recognizing and generating derangements (1 hour)
-- This function tests if a list is the derangement of the first list. This
-- is done by first checking if the lists are permutations en then to call
-- isDerangement2. Also a derangement of an empty list does not exists.
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] _ = False
isDerangement _ [] = False
isDerangement x y
    | isPermutation x y = isDerangement2 x y
    | otherwise = False

-- This function checks if the second list is a derangement by checking if the
-- character of the first list is not on the same position in the second list.
isDerangement2 :: Eq a => [a] -> [a] -> Bool
isDerangement2 [] [] = True
isDerangement2 (x:xs) (y:ys)
    | x == y = False
    | otherwise = isDerangement2 xs ys

-- This function creates a list of derangements by checking all permutations
-- of the list and then check if it is a derangement by calling is Derangement.
deran :: Int -> [[Int]]
deran n = filter (\x -> isDerangement x [0..(n-1)]) (permutations [0..(n-1)])

-- Derangements have the property that a list is not the derangement of itself.
prop5a :: [Int] -> Bool
prop5a x = isDerangement x x == False

-- Derangements have the property that a derangement of a list must have
-- the same length as the list.
prop5b :: [Int] -> [Int] -> Bool
prop5b x y
    | isDerangement x y = (length x == length y)
    | otherwise = True

-- Derangements have the property that if x is a derangement of y,
-- then y is a derangement of x.
prop5c :: [Int] -> [Int] -> Bool
prop5c x y = isDerangement x y == isDerangement y x

-- Derangements have the property that if x is a derangement of y and y is
-- a derangement of z and x not equal to z, then x is a derangement of z.
prop5d :: [Int] -> [Int] -> [Int] -> Bool
prop5d x y z
    | isDerangement x y && isDerangement y z && not (isDerangement x z) && x /= z = False
    | otherwise = True

test5a = do quickCheck prop5a
-- +++ OK, passed 100 tests.
test5b = do quickCheck prop5b
-- +++ OK, passed 100 tests.
test5c = do quickCheck prop5c
-- +++ OK, passed 100 tests.
test5d = do quickCheck prop5d
-- +++ OK, passed 100 tests.

-- Implementing and testing ROT13 (1 hour)
-- This function moves a letter 13 positions in the alphabet.
-- We also need to check for uppercase and 'a' does not start at 1 in the ASCII alphabet.
add13 :: Int -> Int
add13 x
    | x >= 65 && x+13 <= 90 = x+13
    | x >= 65 && x <= 90 = x-13
    | x >= 97 && x+13 <= 122 = x+13
    | x >= 97 && x <= 122 = x-13
    | otherwise = x

-- This function moves all the letters of a string 13 positions.
rot13 :: [Char] -> [Char] -> [Char]
rot13 [] y = y
rot13 (x:xs) y = rot13 xs (y ++ [chr (add13 (ord x))])

-- A postcondition of rot13 is that it is reversable with the same algorithm.
-- So, for example, "Hello" becomes "Uryyb", which should become "Hello" again.
-- If A becomes B, then B becomes A.
prop6a :: [Char] -> Bool
prop6a xs = rot13 (rot13 xs []) [] == xs

-- Another property of rot13 is that it only transforms letters.
-- So it only transforms A-Z en a-z, but not characters like '!' and ':'.
prop6b :: [Char] -> Bool
prop6b xs = prop6b2 xs (rot13 xs [])

prop6b2 :: [Char] -> [Char] -> Bool
prop6b2 [] [] = True
prop6b2 (x:xs) (y:ys)
    | ((xi >= 65 && xi <= 90) || (xi >= 97 && xi <= 122)) && ((yi >= 65 && yi <= 90) || (yi >= 97 && yi <= 122)) && abs (xi-yi) == 13 = prop6b2 xs ys
    | x /= y = False
    | otherwise = prop6b2 xs ys
    where
        xi = ord x
        yi = ord y

-- Furthermore, the input and output should also be of the same length
prop6c :: [Char] -> Bool
prop6c xs = length xs == length ys
    where
        ys = rot13 xs []

test6a = do quickCheck prop6a
-- +++ OK, passed 100 tests.
test6b = do quickCheck prop6b
-- +++ OK, passed 100 tests.
test6c = do quickCheck prop6c
-- +++ OK, passed 100 tests.


-- Implementing and testing IBAN validation (30 min)
-- This function checks if a string is a valid IBAN. This is done by first
-- checking if the string has the right length. Then we need to at the first
-- four chars to the end. Then we translate all the letters in numbers by
-- calling iban2. The total number is then checked to see
-- if mod 97 is equal to 1.
iban :: String -> Bool
iban s
    | length s > 34 && length s < 4 = False
    | mod (read (iban2 (snd (splitAt 4 (s ++ (take 4 s)))) [])) 97 == 1 = True
    | otherwise = False

-- This function translates the letters in the IBAN to numbers.
iban2 :: [Char] -> [Char] -> [Char]
iban2 [] n = n
iban2 (s:ss) n
    | s >= 'A' && s <= 'Z' = iban2 ss (n ++ (show ((ord s) - 55)))
    | otherwise = iban2 ss (n ++ [s])

test7 = iban "GB82WEST12345698765432"
