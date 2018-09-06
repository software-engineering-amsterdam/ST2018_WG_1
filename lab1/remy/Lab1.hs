
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

-- Exercise 1:
ws2left :: Integer -> Integer
ws2left n = foldr (\x y -> (y + x^2)) 0 [1..n]

ws2right :: Integer -> Integer
ws2right n = (n * (n+1) * (2*n + 1)) `div` 6

ws2 :: Integer -> Bool
ws2 n = n >= 0 --> (ws2left n == ws2right n)


ws3left :: Integer -> Integer
ws3left n = foldr (\x y -> (y + x^3)) 0 [1..n]

ws3right :: Integer -> Integer
ws3right n = ((n*(n+1)) `div` 2)^2

ws3 :: Integer -> Bool
ws3 n = n >= 0 --> (ws3left n == ws3right n)


-- Exercise 2:
ws4left :: Integer -> Integer
ws4left n = genericLength (subsequences [1..n])

ws4right :: Integer -> Integer
ws4right n = 2^n

ws4 :: Integer -> Bool
ws4 n = n > 0 --> ws4left n == ws4right n