
module Lab1 where
import Data.List
import Test.QuickCheck    

main = putStrLn "Hello World"

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

-- Exersice 1

exercise1aLeft :: Integer -> Integer
exercise1aLeft n = sum $ map (^2) [1 .. n]

exercise1aRight :: Integer -> Integer
exercise1aRight n = ( n * ( n + 1 ) * ( 2 * n + 1 )) `div` 6

exercise1a :: Integer -> Bool
exercise1a n = exercise1aLeft n == exercise1aRight n

test1a = quickCheckResult (\n -> n >= 0 --> exercise1a n )



exercise1bLeft :: Integer -> Integer
exercise1bLeft n = sum $ map (^3) [1 .. n]

exercise1bRight :: Integer -> Integer
exercise1bRight n = (( n * ( n + 1 )) `div` 2) ^ 2

exercise1b :: Integer -> Bool
exercise1b n = exercise1bLeft n == exercise1bRight n

test1b = quickCheckResult (\n -> n >= 0 --> exercise1b n )


-- Exersice 2

exercise2left :: Integer -> Integer
exercise2left n = genericLength (subsequences [1 .. n])

exercise2right :: Integer -> Integer
exercise2right n = 2 ^ n

exercise2 :: Integer -> Bool
exercise2 n = exercise2left n == exercise2right n

test2 = quickCheckResult (\n -> n >= 0 --> exercise2 n )


-- Exersice 3

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n - 1)

exercise3left :: Integer -> Integer
exercise3left n = genericLength (permutations [1..n])

exercise3right :: Integer -> Integer
exercise3right n = fac n

exercise3 :: Integer -> Bool
exercise3 n = exercise3left n == exercise3right n

test3 = quickCheckResult (\n -> n >= 0 --> exercise3 n )



-- Exersice 4

exercise4help :: Integer -> Integer
exercise4help n = if(prime n && prime (reversal n)) then n else 0

exercise4 :: [Integer]
exercise4 = filter (/=0) (map exercise4help [0 .. 9999])


-- Exersice 5

nextPrime :: Integer -> Integer
nextPrime n = if prime n then n else nextPrime (n+1)

append :: a -> [a] -> [a]
append elem stack = (stack ++ [elem])

pop :: [a] -> [a]
pop [] = error "Can't pop from an empty stack!"
pop (x:xs) = xs

updateList :: [Integer] -> [Integer]
updateList x = append (nextPrime ((last n) + 1)  ) n where n = (pop x)

createList :: [Integer]
createList = take 101 primes

exercise5 :: [Integer] -> Integer
exercise5 _ = exercise5 createList
exercise5 x 
    | prime (sum x) = sum x
    | not (prime (sum x)) = exercise5 (updateList x)


-- Exersice 6

exercise6 testssss
