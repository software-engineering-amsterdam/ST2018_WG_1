
module Lab1 where
import Data.List
import Test.QuickCheck
import Data.Char


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

nextPrime :: Integer -> Integer
nextPrime n = if prime n then n else nextPrime (n+1)







-- Exercise 1
-- Workshop 2
-- To run do test1

left1 :: Int -> [Int] -> Int
left1 x [] = x
left1 x (y:ys) = left1 (x + y^2) ys

right1 :: Int -> Int
right1 n = (n*(n+1)*(2*n+1)) `div` 6

eq1 :: Int -> Bool
eq1 = \xs -> left1 0 [0..xs] == right1 xs

test1 = quickCheckResult (\n -> n >= 0 --> eq1 n)

-- Workshop 3
-- To run do test1w3

left1w3 :: Int -> [Int] -> Int
left1w3 x [] = x
left1w3 x (y:ys) = left1w3 (x + y^3) ys

right1w3 :: Int -> Int
right1w3 n = ((n*(n+1)) `div` 2)^2

eq1w3 :: Int -> Bool
eq1w3 = \xs -> left1w3 0 [0..xs] == right1w3 xs

test1w3 = quickCheckResult (\n -> n >= 0 --> eq1w3 n)


-- Exercise 2
-- To run do test2

-- It is hard to test because there are many subsequence that
-- all have to be calculated. When performing this test we are actually
-- testing if subsequences satify part of its specification.

left2 :: Int -> Int
left2 n = length (subsequences [1..n])

right2 :: Int -> Int
right2 n = 2^n

eq2 :: Int -> Bool
eq2 = \xs -> left2 xs == right2 xs

test2 = quickCheckResult (\n -> n >= 0 --> eq2 n)


-- Exercise 3
-- To run do test3

-- It is hard to test because there are many permutations that
-- all have to be calculated. When performing this test we are actually
-- testing if permutations satify part of its specification.

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

test3 = quickCheckResult(\n -> n >= 1 --> f9 n == fact n)


-- Exercise 4
-- To run do test4
-- It is quite hard to confirm this quistion by code,
-- but if we create a list with all primes lower than 10000 we could find by hand all the reversable primes.
-- And these can be checked with our implementation below.

exercise4help :: Integer -> Integer
exercise4help n = if(prime n && prime (reversal n)) then n else 0

test4 :: [Integer]
test4 = filter (/=0) (map exercise4help [2 .. 10000])



-- Exercise 5
-- To run do test5


sumconsecprimes2 :: [Integer] -> Integer
sumconsecprimes2 x = if prime $ sum $ take 101 x then sum $ take 101 x else sumconsecprimes2 (drop 1 x)

test5 :: Integer
test5 = sumconsecprimes2 primes

-- Exercise 6
conjecturePrime :: [Integer] -> [Integer]
conjecturePrime p
    | prime ((product p) + 1) = conjecturePrime (p ++ [nextPrime (last p + 1)])
    | otherwise = p

test6 = conjecturePrime [2]



-- Exercise 7:
data Card = AmericanExpress | Master | Visa

luhn :: Integer -> Bool
luhn n = luhnSum n `mod` 10 == 0
    where
        luhnSum c       = dig $ map digitToInt $ reverse $ show c
        dig []          = 0
        dig [x]         = x
        dig (x:y:xs)    = x + ((2*y) `div` 10) + ((2*y) `mod` 10) + dig xs

goodLength :: Card -> String -> Bool
goodLength AmericanExpress n    = length n == 15
goodLength Master n             = length n == 16
goodLength Visa n               = length n > 0 && length n <= 19

goodPrefix :: Card -> String -> Bool
goodPrefix AmericanExpress n    = let pre = take 2 n
                                      in pre == "34" || pre == "37"
goodPrefix Master n             = let pre = read $ take 6 n
                                      in elem pre $ [510000..559999] ++ [222100..272099]
goodPrefix Visa n               = head n == '4'

validCard :: Card -> Integer -> Bool
validCard c n = all (== True) [goodLength c cn, goodPrefix c cn, luhn n]
    where cn = show n

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress n = validCard AmericanExpress n
isMaster n          = validCard Master n
isVisa n            = validCard Visa n



-- Exercise 8:
-- Someone is guilty if 3 persons say so, because only 2 persons are lying.
-- The third person is then always telling the truth.
-- Everyone who accuses the guilty person is honest.

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && (not a || not b)

accuses :: Boy -> Boy -> Bool
accuses Matthew n   = n /= Carl && n /= Matthew
accuses Peter n     = n == Matthew || n == Jack
accuses Jack n      = not $ accuses Matthew n || accuses Peter n
accuses Arnold n    = xor (accuses Matthew n) (accuses Peter n)
accuses Carl n      = not $ accuses Arnold n

accusers :: Boy -> [Boy]
accusers x = [i | i <- boys, accuses i x]

guilty, honest :: [Boy]
guilty = [i | i <- boys, length (accusers i) >= 3]
honest = concat [accusers x | x <- guilty]
