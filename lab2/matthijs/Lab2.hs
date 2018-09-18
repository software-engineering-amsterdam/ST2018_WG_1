
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



checkprobs :: Int -> IO [Int]
checkprobs n = do
  list <- probs n
  return $ checkCount list
  where
    checkCount l = [first l, second l, third l, fourth l]
    first l = length (filter (\x -> 0 < x && x < 0.25) l)
    second l = length (filter (\x -> 0.25 <= x && x < 0.5) l)
    third l = length (filter (\x -> 0.5 <= x && x < 0.75) l)
    fourth l = length (filter (\x -> 0.75 <= x && x < 1) l)


triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
          | a + b <= c || a + c <= b || b + c <= a = NoTriangle
          | a == b && b == c = Equilateral
          | a == b || b == c || c == a = Isosceles
          | (a*a + b*b == c*c) || (b*b + c*c == a*a) || (a*a + c*c == b*b) = Rectangular
          | otherwise = Other

knownTriangles :: Integer -> Integer -> Integer -> Shape
knownTriangles 1 1 3 = NoTriangle
knownTriangles 8 2 2 = NoTriangle
knownTriangles 2 2 3 = Equilateral
knownTriangles 3 4 4 = Equilateral
knownTriangles 5 5 5 = Isosceles
knownTriangles 3 4 5 = Rectangular
knownTriangles 3 6 9 = Other



------

forall :: [a] -> (a->Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p


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



descliststrength :: IO()
descliststrength = descliststrength2 funclist [] [] funcliststring [] []

descliststrength2 :: [(Int -> Bool)] -> [(Int -> Bool)] -> [(Int -> Bool)] -> [String] -> [String] -> [String] -> IO()
descliststrength2 (x:y:funclist) m n (xs:ys:funcliststring) ms ns
                              | mystronger x y = descliststrength2 (y:funclist) (x:m) n (ys:funcliststring) (xs:ms) ns
                              | mystronger y x = descliststrength2 (x:funclist) (y:m) n (xs:funcliststring) (ys:ms) ns
--if (mystronger x y) then descliststrength (y:funclist) (x:m) n else descliststrength (x:funclist) (y:m) n
descliststrength2 [x] m n [xs] ms ns = descliststrength2 m [] (x:n) ms [] (xs:ns)
descliststrength2 [] _ n _ _ ns = print ns


isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation x y = if (length x == length y) then isPermutation2 x y [] else False


isPermutation2 :: Eq a => [a] -> [a] -> [a] -> Bool
isPermutation2 (x:xs) (y:ys) n
                      | x == y = isPermutation2 xs (ys ++ n) []
                      | otherwise = isPermutation2 (x:xs) ys (y:n)
isPermutation2 [] [] _ = True
isPermutation2 _ [] _ = False


isDerangement, isDerangement2 :: Eq a => [a] -> [a] -> Bool
isDerangement x y = if (isPermutation x y) then isDerangement2 x y else False
isDerangement2 (x:xs) (y:ys) = if (x == y) then False else isDerangement2 xs ys
isDerangement2 [] [] = True


deran :: Int -> [[Int]]
deran n = filter(\x -> isDerangement x [0..(n-1)]) (permutations [0..(n-1)])

rot13 :: String -> String
rot13 n = map (\x -> if((ord x + 13) > 122) then (chr $ ord x -13) else chr $ ord x + 13) n


--listIntToInt :: [Int] -> Int
--listIntToInt x = read . concatMap show x

--iban :: String -> [Int]
--iban (x1:x2:x3:x4:n) =   map (\x -> if(isDigit x) then (digitToInt x) else (ord x - 55)) (n ++  [x1,x2,x3,x4])


iban3 :: String -> Int
iban3 (x1:x2:x3:x4:n) = (iban2 (n ++  [x1,x2,x3,x4]) 0) 



iban2 :: String -> Int -> Int
iban2 [] n = n
iban2 (x:xs) n = if(isDigit x) then iban2 xs (n*10 + digitToInt x) else iban2 xs (n*100 + (ord x - 55))
