
module Lab1 where
import Data.List
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Integer -> Integer
reversal = read . reverse . show

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

-- Exercise 1
-- Workshop 2
left1 :: Int -> [Int] -> Int
left1 x [] = x
left1 x (y:ys) = left1 (x + y^2) ys

right1 :: Int -> Int
right1 n = (n*(n+1)*(2*n+1)) `div` 6

eq1 :: Int -> Bool
eq1 = \xs -> left1 0 [0..xs] == right1 xs

test1 = quickCheckResult (\n -> n >= 0 --> eq1 n)

-- Workshop 3
left2 :: Int -> [Int] -> Int
left2 x [] = x
left2 x (y:ys) = left2 (x + y^3) ys

right2 :: Int -> Int
right2 n = ((n*(n+1)) `div` 2)^2

eq2 :: Int -> Bool
eq2 = \xs -> left2 0 [0..xs] == right2 xs

test2 = quickCheckResult (\n -> n >= 0 --> eq2 n)





