module Lab6 where

import Data.List
import Data.Bits
import System.Random
import Test.QuickCheck
import Lecture6

-- Exercise 1: (2:00)
exM2 :: Integer -> Integer -> Integer -> Integer
exM2 b e m = exM' (b `mod` m) e m 1
    where
        exM' b 0 m r = r
        exM' b e m r
            | even e    = exM' (b*b `mod` m) (e `div` 2) m r
            | otherwise = exM' (b*b `mod` m) (e `div` 2) m (r*b `mod` m)


exM3 :: Integer -> Integer -> Integer -> Integer
exM3 _ 0 _ = 1
exM3 b e m = t * (exM3 (b*b `mod` m) (shiftR e 1) m) `mod` m
    where
        t = if testBit e 0 then b `mod` m else 1


exM4 :: Integer -> Integer -> Integer -> Integer
exM4 _ 0 _ = 1
exM4 b e m = exM4' b e m
    where
        exM4' _ 0 _ = 1
        exM4' b e m = ((if (testBit e 0) then (b `mod` m) else 1) * (exM4' ((b*b) `mod` m) (shiftR e 1) m)) `mod` m


-- Exercise 2: (0:30)
-- See lukas
nTimes :: (Integer -> Integer -> Integer -> Integer) -> Integer -> 
          Integer -> Integer -> Integer -> Integer -> Integer
nTimes f b e m 0 r = r
nTimes f b e m n r = nTimes f b e m (n-1) (f b e m)

{--
*Lab6> exM 582759 9358326 392847364     --> 342255341   | (0.01 secs, 88,832 bytes)
*Lab6> expM 582759 9358326 392847364    --> 342255341   | (1.52 secs, 66,660,216 bytes)

*Lab6> exM 582759 93583269 392847364    --> 284945123   | (0.01 secs, 90,520 bytes)
*Lab6> expM 582759 93583269 392847364   --> 284945123   | (17.43 secs, 636,398,208 bytes)

The new function for modular exponentiation is faster and
requires less allocated bytes in both cases.

While the original function takes longer to compute the second case, the new
function requires somewhat the same time and bytes.

--}


-- Exercise 3: (0:30)
-- See lukas
composite = filter (not . prime) [4..]


-- Exercise 4: (2:30)
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