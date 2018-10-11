module Lab6 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture6


-- Exercise 1 (30 min)
-- This is the function for modular exponetiation and it can be
-- found in Lecture6.hs
{--
exM :: Integer -> Integer -> Integer -> Integer
exM b e m = exM' (b `mod` m) e m 1
    where
        exM' b 0 m r = r
        exM' b e m r
            | even e    = exM' (b*b `mod` m) (e `div` 2) m r
            | otherwise = exM' (b*b `mod` m) (e `div` 2) m (r*b `mod` m)
--}

-- Exercise 2 (30 min)
nTimes :: (Integer -> Integer -> Integer -> Integer) -> Integer ->
        Integer -> Integer -> Integer -> Integer -> Integer
nTimes f b e m 0 r = r
nTimes f b e m n r = nTimes f b e m (n-1) (f b e m)

{--
In haskell we can use :set +s to see how long a function takes.
When we do this for both nTimes for both functions we can look
at the difference.

:set +s
nTimes exM 2133231 1231231 345 10000 0
Lecture6> 96
Lecture6> (0.01 secs, 2,320,496 bytes)

:set +s
nTimes expM 2133231 1231231 345 10000 0
Lecture6> 96
Lecture6> (0.38 secs, 11,734,248 bytes)

This test shows that exM is faster and uses less bytes than expM.
--}

-- Exercise 3 (30 min)
-- Composite numbers are positive numbers that can be divided by more
-- numbers then only 1 and the number itself. Because of this composite
-- numbers are all number higher then 1 that are not prime. We start with
-- 4 because 2 and 3 are prime.
composites :: [Integer]
composites = filter (not.prime) [4..]

-- Exercise 4 ()
-- The Fermats prime test uses random numbers, below the number we
-- want to check, to check wether the imput number is prime.
-- The primeTestsF checks if k ammount of random nunbers below the
-- input number confirm that the input number is prime. When k
-- gets higher we check with more random numbers and thus the
-- chance that a composite number is labeled as a prime number
-- is smaller.




--ex5
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
          k <- [2..],
          prime (6*k+1),
          prime (12*k+1),
          prime (18*k+1)]

-- Al tested Carmichael numbers test true with Fermats primality test.
-- Carmichael numbers have the property we test with Fermats little theorem,
-- namely that if p is a prime number, then for any int b, the number b^p - b
-- is an int multiple of p.
-- https://en.wikipedia.org/wiki/Carmichael_number

ex5 :: IO()
ex5 = ex5' carmichael

ex5' :: [Integer] -> IO()
ex5' (x:xs) = do
  print(x)
  res <- primeTestsF 10 x
  print(res)
  ex5' xs


-- ex6

-- All tested Carmichael numbers test false with MR primality test.
-- MR weeds out (most) Carmichael numbers because it also tests for a
-- non-trivial root of unity.
-- https://cs.stackexchange.com/questions/21462/why-miller-rabin-instead-of-fermat-primality-test

ex6 :: IO()
ex6 = ex6' carmichael

ex6' :: [Integer] -> IO()
ex6' (x:xs) = do
  print(x)
  res <- primeMR 10 x
  print(res)
  ex6' xs


-- ex6_2

-- Checked with list at https://en.wikipedia.org/wiki/Mersenne_prime
-- All found primes appear to be correct Mersenne primes

ex62 :: IO()
ex62 = ex62' primes

ex62' :: [Integer] -> IO()
ex62' (x:xs) = do
  isPrime <- primeMR 10 (2^x -1)
  if(isPrime) then print (2^x -1) else return ()
  ex62' xs