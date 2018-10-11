module Lab6 where

import Data.List
import System.Random
import Test.QuickCheck

-- Exercise 1: 
exM :: Integer -> Integer -> Integer -> Integer
exM b e m = exM' (b `mod` m) e m 1
    where
        exM' b 0 m r = r
        exM' b e m r
            | even e    = exM' (b*b `mod` m) (e `div` 2) m r
            | otherwise = exM' (b*b `mod` m) (e `div` 2) m (r*b `mod` m)


ex1Perf :: Integer -> Integer -> Integer -> Integer -> IO ()
ex1Perf b e m 0 = putStrLn "Finished"
ex1Perf b e m n = do
    exM b e m
    return $ ex1Perf b e m (n-1)

mExpPerf :: Integer -> Integer -> Integer -> Integer -> IO ()
mExpPerf b e m 0 = putStrLn "Finished"
mExpPerf b e m n = do
    b^e `mod` m
    return $ mExpPerf b e m (n-1)
