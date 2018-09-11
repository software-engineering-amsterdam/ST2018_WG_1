module Lab1 where

import Data.List
import Test.QuickCheck

sentence = "Sentences can go " ++ onAndOn
onAndOn  = "on and " ++ onAndOn

sentences = "Sentences can go on":
             map (++ " and on") sentences

threefold :: Integer -> Bool
threefold n = rem n 3 == 0

threefolds = filter threefold [0..]

nats = [0..]

existsLargestNatural = all (\ n -> any (\ m -> n < m) nats) nats

existsSmallestNatural = any (\ n -> all (\ m -> n <= m) nats) nats

forall = flip all
exist  = flip any

existsLargestNatural' = forall nats (\ n -> exist nats (\ m -> n < m))

existsSmallestNatural' = exist nats (\ n -> forall nats (\ m -> n <= m))

myall :: (a -> Bool) -> [a] -> Bool
myall p [] = True
myall p (x:xs) = p x && myall p xs

list2p :: Eq a => [a] -> a -> Bool
list2p = flip elem

myallTest :: [Int] -> [Int] -> Bool
myallTest = \ ys xs -> let p = list2p ys in
  all p xs == myall p xs

myall' p = foldr (\ x b -> p x && b) True

myallTest' :: [Int] -> [Int] -> Bool
myallTest' = \ ys xs -> let p = list2p ys in
  all p xs == myall' p xs

divide :: Integer -> Integer -> Bool
divide n m = rem m n == 0

isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\ d -> not (divide d n)) [2..n-1]

isPrime' :: Integer -> Bool
isPrime' n = all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) [2..]

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

sieve :: [Integer] -> [Integer]
sieve (n:ns) = n : sieve (filter (\m -> rem m n /= 0) ns)

eprimes = sieve [2..]

least :: (Integer -> Bool) -> Integer

least p = head (filter p nats)

least1 p = lst p 0
     where lst p n = if p n then n else lst p (n+1)

dif2 :: [Integer] -> [(Integer,Integer)]
dif2 (p:q:rs) = if p + 2 == q then (p,q) : dif2 (q:rs)
                else dif2 (q:rs)

primePairs = dif2 primes

dif6 :: [Integer] -> [(Integer,Integer,Integer)]
dif6 (p:q:r:ss) = if p + 6 == r then (p,q,r) : dif6 (q:r:ss)
                  else dif6 (q:r:ss)

primeTriples = dif6 primes

sol = take 100 primeTriples

nextPrime :: Integer -> Integer
nextPrime n = if prime n then n else nextPrime (n+1)

mersenne :: [Integer]
mersenne = [ p | p <- primes, prime (2^p - 1) ]

pythTriples :: [(Integer,Integer,Integer)]
pythTriples = filter (\ (x,y,z) -> x^2 + y^2 == z^2)
   [ (x,y,z) | z <- [1..], x <- [1..z], y <- [1..z], x < y  ]

seq1 k = map sum [ [1..n] | n <- [0..k] ]
seq2 k = map (\ n -> (n * (n+1)) `div` 2) [0..k]

seq1seq2Test = \ n -> seq1 n == seq2 n

f1, f2 :: Int -> Int
f1 = \ n -> sum [0..n]
f2 = \ n -> (n*(n+1)) `div` 2

test1 = quickCheckResult (\n -> f1 n == f2 n)

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

test1' = quickCheckResult (\n -> n >= 0 --> f1 n == f2 n)

sq :: Int -> Int
sq = \ n -> n*n

f3, f4 :: Int -> Int
f3 = \ n -> sum $ map sq [1..n]
f4 = \ n -> (n*(n+1)*(2*n+1)) `div` 6

test2 = quickCheckResult (\n -> n >= 0 --> f3 n == f4 n)


sq3 :: Int -> Int
sq3 = \ n -> n*n*n

f5, f6 :: Int -> Int
f5 = \ n -> sum $ map sq3 [1..n]
f6 = \ n -> sq ((n*(n+1)) `div` 2)

test3 = quickCheckResult (\n -> n >= 0 --> f5 n == f6 n)


f7, f8 :: Int -> Int
f7 = \ n -> length [1..n]
f8 = \ n -> length (subsequences [1..n])

test4 = quickCheckResult (\n -> n>= 1 --> 2^(f7 n) == f8 n)


perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

f9 :: Int -> Int
f9 = \ n -> length (perms [1..n])

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

test5 = quickCheckResult(\n -> n >= 1 --> f9 n == fact n)


reversal :: Integer -> Integer
reversal = read . reverse . show

revprime :: Integer -> Bool
revprime n = prime n && prime (reversal n)

revprimes :: [Integer]
revprimes = 2 : filter revprime [3..10000]


sumconsecprimes :: Integer
sumconsecprimes = sumconsecprimes2 primes

sumconsecprimes2 :: [Integer] -> Integer
sumconsecprimes2 x = if prime $ sum $ take 101 x then sum $ take 101 x else sumconsecprimes2 (drop 1 x)


conjconsecprimes :: [Integer]
conjconsecprimes = conjconsecprimes2 2

conjconsecprimes2 :: Int -> [Integer]
conjconsecprimes2 x = if not $ prime $ product (take x primes) + 1 then (take x primes) else conjconsecprimes2 (x+1)


luhn :: Integer -> Bool
luhn n = (luhn2 n 0) `mod` 10 == 0

luhn2 :: Integer -> Integer -> Integer
luhn2 0 s = s
luhn2 n s = luhn2 (n `div` 100) (s + (n `mod` 10) + luhn3 ((n `div` 10) `mod` 10))

luhn3 :: Integer -> Integer
luhn3 n = if n * 2 > 9 then (n * 2) - 9 else n * 2
