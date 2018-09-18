
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q



data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)


-- Redcurry
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)


redcurryTest :: Int -> IO [Int]
redcurryTest n = do
    list <- probs n
    return [length (first list), length (second list), length (thirth list), length (fourth list)]
    where
        first l = [y | y <- l, y >= 0 && y <= 0.25 ]
        second l = [y | y <- l, y > 0.25 && y <= 0.5 ]
        thirth l = [y | y <- l, y > 0.5 && y <= 0.75 ]
        fourth l = [y | y <- l, y > 0.75 && y <= 1 ]


-- exercise 2
triangle :: Integer -> Integer -> Integer -> Shape
triangle n o p 
    | ((( (n + o) < p) || ( (n + p) < o)) || ( (p + o) < n)) = NoTriangle
    | ((n == o) && (o == p)) = Equilateral
    | ((( (n == o)) || ( (n == p))) || ( (p == o))) = Isosceles
    | ((( (n^2 + o^2) == p^2) || ( (n^2 + p^2) == o^2)) || ( (p^2 + o^2) == n^2)) = Rectangular
    | otherwise = Other


-- exercise 3



forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

compar :: [a] -> (a -> Bool) -> (a -> Bool) -> String
compar xs p q = let pq = stronger xs p q 
                    qp = stronger xs q p 
                in 
                  if pq && qp then "equivalent"
                  else if pq  then "stronger"
                  else if qp  then "weaker"
                  else             "incomparable"


-- f1, f2 :: Int -> Bool
-- f1 = (\ x -> even x && x > 3) 
-- f2 = (\ x -> even x || x > 3)
-- f3 = (\ x -> (even x && x > 3) || even x)
-- f4 = even (\ x -> (even x && x > 3) || even x)

-- test1 = compar [-10..10] (\ x -> even x && x > 3) even
-- test2 = compar [-10..10] (\ x -> even x || x > 3) even
-- test3 = compar [-10..10] (\ x -> (even x && x > 3) || even x) even
-- test4 = compar [-10..10] even (\ x -> (even x && x > 3) || even x)

prop1 = (\ x -> even x && x > 3 )
prop2 = (\ x -> even x || x > 3 )
prop3 = (\ x -> (even x && x > 3) || even x )
prop4 = even

test1 = compar [-10..10] prop1 prop2
test2 = compar [-10..10] prop1 prop3
test3 = compar [-10..10] prop1 prop4
test4 = compar [-10..10] prop2 prop3
test5 = compar [-10..10] prop2 prop4
test6 = compar [-10..10] prop3 prop4


countL :: Eq a => a -> [a] -> Int
countL x = length . filter (x==)

-- countList :: [a] -> [(a, Int)]
-- countList list = 
listCount :: [a] -> [a]
listCount a :: [ [countL y a, y] | y <- (nub a) ]

compareL :: Eq a => [a] -> [a] -> Bool
compareL (x:xs) b
    | find x
    |

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = result
    where result = compareL (listCount a) b
