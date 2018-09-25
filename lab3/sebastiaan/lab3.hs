module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3


-- Exersice 1 

contradiction :: Form -> Bool
contradiction f = not $ satisfiable f

form4 = Equiv p (Neg p)

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

form5 = Dsj [p, (Neg p)]

 -- | logical entailment 
entails :: Form -> Form -> Bool
entails f v = all (\ x -> evl x f) (filter (\ x -> evl x v) (allVals v))

 -- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f v = (filter (\ x -> evl x v) (allVals v)) == (filter (\ x -> evl x f) (allVals f))


-- Exercise 2



-- Exercise 3

distributeLaw :: Form -> Form
distributeLaw (Prop x) = Prop x
-- (P⋁(Q⋀R))↔(P⋁Q)⋀(P⋁R)
distributeLaw (Dsj [x, Cnj[y, z]]) = Cnj [distributeLaw (Dsj [x, y]), distributeLaw (Dsj [x, z])]
distributeLaw (Dsj [Cnj[y, z], x]) = Cnj [distributeLaw (Dsj [y, x]), distributeLaw (Dsj [z, x])]
distributeLaw (Cnj f) = Cnj (map distributeLaw f)
distributeLaw (Dsj f)
    | mappedf /= f = distributeLaw (Dsj (mappedf))
    | otherwise = Dsj (mappedf)
    where mappedf = map distributeLaw f
distributeLaw x = x

checkCNF :: Form -> Bool
checkCNF f = equiv f (makeCnf f)

makeCnf :: Form -> Form
makeCnf f = distributeLaw $ nnf $ arrowfree f

makeCnf2 :: Form -> Form
makeCnf2 f = nnf $ arrowfree f

-- Exercise 4

getRandomInt :: Int -> Int -> IO Int
getRandomInt left right = getStdRandom (randomR (left, right))

genRandomForm' :: Int -> IO Form
genRandomForm' 0 = do
    a <- (getRandomInt (-10) 10)
    return (Prop a)
genRandomForm' counter = do
    a <- (getRandomInt 1 5)
    prop1 <- (genRandomForm' (counter - 1))
    prop2 <- (genRandomForm' (counter - 1))
    case a of 
        1 -> do
            return (Cnj [prop1, prop2])
        2 -> do
            return (Dsj [prop1, prop2])
        3 -> do
            return (Impl prop1 prop2)
        4 -> do
            return (Equiv prop1 prop2)
        5 -> do
            x <- (getRandomInt (-10) 10)
            return (Prop x)


getRandomForm :: IO Form
getRandomForm = do
    a <- (getRandomInt 0 3)
    f <- (genRandomForm' a)
    return f
