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
In haskell we can use ':set +s' to see how long a function takes.
When we do this with nTimes for both functions we can look
at the difference between the two functions.

:set +s
nTimes exM 2133231 1231231 345 10000 0
Lecture6> 96
Lecture6> (0.01 secs, 2,320,496 bytes)

:set +s
nTimes expM 2133231 1231231 345 10000 0
Lecture6> 96
Lecture6> (0.38 secs, 11,734,248 bytes)

We also tested the difference for a single computation, but with large numbers:
*Lab6> exM 582759 9358326 392847364     --> 342255341   | (0.01 secs, 88,832 bytes)
*Lab6> expM 582759 9358326 392847364    --> 342255341   | (1.52 secs, 66,660,216 bytes)

*Lab6> exM 582759 93583269 392847364    --> 284945123   | (0.01 secs, 90,520 bytes)
*Lab6> expM 582759 93583269 392847364   --> 284945123   | (17.43 secs, 636,398,208 bytes)

The new function for modular exponentiation is faster and
requires less allocated bytes in both cases.

While the original function takes longer to compute the second case, the new
function requires somewhat the same time and bytes.
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
-- The primeTestsF checks if k amount of random numbers below the
-- input number confirm that the input number is prime. When k
-- gets higher we check with more random numbers and thus the
-- chance that a composite number is labeled as a prime number
-- is smaller.

{--
RESULTS:

k=1, n=1000 smallest=9  highest=341     avg=38      | (0.21 secs, 197,131,240 bytes)
k=2, n=1000 smallest=9  highest=2821    avg=398     | (3.66 secs, 3,270,017,632 bytes)
k=3, n=1000 smallest=9  highest=15841   avg=1457    | (16.48 secs, 15,092,858,920 bytes)
k=4, n=1000 smallest=9  highest=101101  avg=3480    | (45.26 secs, 44,210,532,072 bytes)
k=5, n=1000 smallest=15 highest=162401  avg=5709    | (79.39 secs, 80,692,664,136 bytes)
k=6, n=1000 smallest=65 highest=252601  avg=10497   | (156.51 secs, 164,737,750,840 bytes)

When k is increased, the smallest number we find is for every tested k almost the same.
However, the highest number increases significantly for every increase in k.
This is because with a higher k, it's less likely to find a number that is probably prime.
So sometimes it takes a lot of numbers to find one that's seen as prime and with
a higher k this happens more often.
--}

-- This function does the fermat test on all composite numbers
-- until we find a number that is prime (and fools the fermat test).
ex4 :: Int -> [Integer] -> IO Integer
ex4 k (x:xs) = do
    isPrime <- primeTestsF k x
    if isPrime then return x else ex4 k xs

-- This function executes function ex4 n times and returns a list of the results.
ex4NTimes :: Int -> Int -> IO [Integer]
ex4NTimes k n = ex4NTimes' k n composite []
    where
        ex4NTimes' _ 0 _ r = return r
        ex4NTimes' k n l r = do
            p <- ex4 k l
            ex4NTimes' k (n-1) l $ r ++ [p]

-- This function returns the smallest composite number that fooled the fermat test for a given k.
ex4Smallest :: Int -> Int -> IO Integer
ex4Smallest k n = do
    r <- ex4NTimes k n
    return $ head $ sort r

-- This function does all the things above and prints all interesting data.
ex4PrintAll :: Int -> Int -> IO ()
ex4PrintAll k n = do
    l               <- ex4NTimes k n
    let sl          =  sort l
    let smallest    =  head sl
    let highest     =  last sl
    let avg         =  (sum sl) `div` (genericLength sl)
    putStrLn ("k=" ++ show k ++ ", n=" ++ show n ++ "\tsmallest=" ++
              show smallest ++ "\thighest=" ++ show highest ++ "\tavg=" ++ show avg)


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