
module Lab1 where
import Data.List
import Data.Char
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

nextPrime :: Integer -> Integer
nextPrime n = if prime n then n else nextPrime (n+1)



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
-- It is hard to test, because when n gets pretty large 
-- your list with all subsequences becomes very big.
-- This causes a stack overflow and it'll stop the test.

ws4left :: Integer -> Integer
ws4left n = genericLength (subsequences [1..n])

ws4right :: Integer -> Integer
ws4right n = 2^n

ws4 :: Integer -> Bool
ws4 n = n >= 0 --> ws4left n == ws4right n


-- Exercise 3:
-- It is hard to test, because when n gets pretty large 
-- your list with all subsequences becomes very big.
-- This causes a stack overflow and it'll stop the test.

ws5left :: Integer -> Integer
ws5left n = genericLength (permutations [1..n])

ws5right :: Integer -> Integer
ws5right n = fac n

fac :: Integer -> Integer
fac 0 = 1
fac n = n * (fac (n-1))

ws5 :: Integer -> Bool
ws5 n = n >= 0 --> ws5left n == ws5right n

-- Exercise 4:
getPrimes :: Integer -> [Integer]
getPrimes n = 2 : filter prime [3..n]

reversePrime :: Integer -> Integer
reversePrime p = (if prime (reversal p) then p else 0)

ex4 :: [Integer]
ex4 = filter (/= 0) (map reversePrime (getPrimes 10000))


-- Exercise 5:
ex5 :: Integer -> Integer -> Integer -> Integer -> Integer
ex5 0 0 0 0 = ex5 0 2 2 0
ex5 c l h s
    | c < 101                       = ex5 (c+1) l (nextPrime (h+1)) (s+h)
    | c == 101 && not (prime s)     = ex5 c (nextPrime (l+1)) (nextPrime (h+1)) (s+h-l)
    | c == 101 && prime s           = s


-- Exercise 6:
getNPrimes :: Integer -> Integer -> Integer -> [Integer] -> [Integer]
getNPrimes c n p list
    | c == n        = list
    | c < n         = getNPrimes (c+1) n (nextPrime p+1) (list ++ [(nextPrime p)])


ex6 :: Integer -> [Integer]
ex6 n
    | prime ((foldr (*) 1 (getNPrimes 0 n 0 [])) + 1)   = ex6 (n+1)
    | otherwise                                         = getNPrimes 0 n 0 []



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