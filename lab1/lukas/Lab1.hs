
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
left1w3 :: Int -> [Int] -> Int
left1w3 x [] = x
left1w3 x (y:ys) = left1w3 (x + y^3) ys

right1w3 :: Int -> Int
right1w3 n = ((n*(n+1)) `div` 2)^2

eq1w3 :: Int -> Bool
eq1w3 = \xs -> left1w3 0 [0..xs] == right1w3 xs

test1w3 = quickCheckResult (\n -> n >= 0 --> eq1w3 n)

-- Exercise 2
left2 :: Int -> Int
left2 n = length (subsequences [1..n])

right2 :: Int -> Int
right2 n = 2^n

eq2 :: Int -> Bool
eq2 = \xs -> left2 xs == right2 xs

test2 = quickCheckResult (\n -> n >= 0 --> eq2 n)

-- It is hard to test because there are many subsequence that
-- all have to be calculated. When performing this test we are actually
-- testing if subsequences satify part of its specification.

-- Exercise 3
left3 :: Int -> Int
left3 n = length (permutations [1..n])

factorial :: Int -> [Int] -> Int
factorial x [] = x
factorial x (y:ys) = factorial (x*y) ys

eq3 = \xs -> left3 xs == factorial 1 [1..xs]

test3 = quickCheckResult (\n -> n >= 0 --> eq3 n)

-- It is hard to test because there are many permutations that
-- all have to be calculated. When performing this test we are actually
-- testing if permutations satify part of its specification.

-- Exercise 4
reversalPrime :: [Integer] -> [Integer] -> [Integer]
reversalPrime [] p = p
reversalPrime (n:ns) p
    | prime n && prime (reversal n) = reversalPrime ns (p ++ [n])
    | otherwise = reversalPrime ns p

test4 = reversalPrime [2..10000] []

-- Exercise 5
nextPrime :: Integer -> Integer
nextPrime n = if prime n then n else nextPrime (n+1)

consecutivePrime :: [Integer] -> Integer
consecutivePrime p
    | prime (sum p) = sum p
    | otherwise = consecutivePrime ((drop 1 p) ++ [nextPrime (last p + 1)])

test5 = consecutivePrime (take 101 primes)

-- Exercise 6
conjecturePrime :: [Integer] -> [Integer]
conjecturePrime p
    | prime ((product p) + 1) = conjecturePrime (p ++ [nextPrime (last p + 1)])
    | otherwise = p

test6 = conjecturePrime [2]

-- Exercise 7
luhn :: Integer -> Bool
luhn n = luhn2 0 n False

luhn2 :: Integer -> Integer -> Bool -> Bool
luhn2 c 0 t
    | c `mod` 10 == 0 = True
    | otherwise = False
luhn2 c n t
    | t = luhn2 (c + (luhn3 (2*(n `mod` 10)))) (n `div` 10) False
    | otherwise = luhn2 (c + (n `mod` 10)) (n `div` 10) True

luhn3 :: Integer -> Integer
luhn3 n
    | n > 9 = n - 9
    | otherwise = n

isAmericanExpress :: Integer -> Bool
isAmericanExpress n
    | n > 999999999999999 && luhn n && (n `div` (10^14) == 34 ||
                                        n `div` (10^14) == 37) = True
    | otherwise = False

isMaster :: Integer -> Bool
isMaster n
    | n > 999999999999999 && luhn n && (elem (n `div` (10^12)) [2221..2720] ||
                                        elem (n `div` (10^14)) [51..55]) = True
    | otherwise = False

isVisa :: Integer -> Bool
isVisa n
    | n > 999999999999999 && luhn n && (n `div` (10^15)) == 4 = True
    | otherwise = False

test7a = luhn 79927398713
test7b = luhn 79927398714
test7c = isAmericanExpress 3400000000000000
test7d = isMaster 5500000000000004
test7e = isVisa 4111111111111111

-- Exercise 8
data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]







