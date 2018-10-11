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

