module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- ex1

-- Contradiction if not satisfiable
contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

-- Contradiction to test
form4 = Cnj [p, (Neg p)]

-- Tautology if always returns true
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- Tautology to Test
form5 = Dsj [p, (Neg p)]

-- | logical entailment
entails :: Form -> Form -> Bool
entails f1 f2 =  all(\ v -> evl v f2 == True) (filter (\ v -> evl v f1 == True) (allVals f1))

-- Equivalent with form4, used to test equivalence
form7 = Cnj [(Neg p), p]


-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 = map(\ v -> evl v f1) (allVals f1) == map(\ v -> evl v f2) (allVals f2)


-- ex2
badFormString1 = "*(1 + (2 -3)"
badFormString2 = "*(1 + (2 - 3))))"
parseTest1 f = head(parse (show f)) == f
parseTest2 = parse(badFormString1) == []
--parseTest3 = show $ parse(badFormString2) == (*(1 +(2 -3)))


--ex3
cnf :: Form -> Form
cnf f = cnfHelper3 $ nnf $ arrowfree f

--cnfHelper :: Form -> Form
--cnfHelper (Cnj (x1:x2)) = if(x1 == head x2) then Prop x1 else cnfHelper (Cnj (x1:x2))
--cnfHelper (Dsj (x1:x2)) = if(x1 == head x2) then Prop x1 else cnfHelper (Dsj (x1:x2))
--cnfHelper (Cnj (x1:x2)) = cnfHelper (Cnj (x2:x1))
--cnfHelper (Dsj (x1:x2)) = cnfHelper (Dsj (x2:x1))

cnfHelper3 :: Form -> Form
cnfHelper3 (Prop x) = Prop x
cnfHelper3 (Neg (Prop x)) = Neg (Prop x)
cnfHelper3 (Dsj [x1,(Cnj [x21, x22])]) = Cnj [(cnfHelper2(Dsj[cnfHelper2(x1), cnfHelper2(x21)])), cnfHelper2(Dsj[cnfHelper2(x1), cnfHelper2(x22)])]
cnfHelper3 (Dsj [(Cnj [x21, x22]), x1]) = Cnj [(cnfHelper2(Dsj[x1, x21])), cnfHelper2(Dsj[x1, x22])]
cnfHelper3 (Cnj [(Cnj [x11, x12]), (Cnj [x21,x22])]) = Cnj[x11,x12,x21,x22]



cnfHelper2 :: Form -> Form
--cnfHelper2 (Dsj x) = Dsj x
cnfHelper2 (Prop x) = Prop x
cnfHelper2 (Neg (Prop x)) = Neg (Prop x)
--cnfHelper2 (Cnj x) = Cnj (map cnfHelper2 x)
cnfHelper2 (Dsj [x1,(Cnj(x21:x22))]) = Cnj([(cnfHelper2(Dsj(x1 : [x21])))] ++ [(cnfHelper2(Dsj(x1 : x22)))])
cnfHelper2 (Dsj [(Cnj(x21:x22)),x1]) = Cnj([(cnfHelper2(Dsj(x1 : [x21])))] ++ [(cnfHelper2(Dsj(x1 : x22)))])
cnfHelper2 (Cnj [(Cnj(x11:x12)),(Cnj(x21:x22))]) = cnfHelper2 (Cnj(([x11] ++ x12) ++ ([x21] ++ x22)))
--cnfHelper2 (Cnj [(Cnj(x11:x12)),y]) = cnfHelper2 (Cnj(([x11] ++ x12) ++ [y]))
--cnfHelper2 (Cnj [y,(Cnj(x11:x12))]) = cnfHelper2 (Cnj(([x11] ++ x12) ++ [y]))
--cnfHelper2 (Cnj (x1:[Cnj(x2:x3)])) = Cnj([cnfHelper2(x1)] ++ [cnfHelper2(x2)] ++ map cnfHelper2 x3)
cnfHelper2 (Cnj (Cnj(x2:x3):x1)) = Cnj( map cnfHelper2 x3 ++ [cnfHelper2(x2)] ++ map cnfHelper2 x1)
cnfHelper2 (Cnj (x1:xs)) = Cnj ([cnfHelper2(x1)] ++ map cnfHelper2 xs)
--cnfHelper2 (Cnj (x1:xs)) = Cnj ([cnfHelper2(x1)] ++ map cnfHelper2 xs)
--cnfHelper2 (Cnj (x1:x2:xs)) = cnfHelper2(Cnj((mergeCnj(x1:x2)):xs))
cnfHelper2 (Cnj (x1:xs)) = Cnj([cnfHelper2(x1)] ++ map cnfHelper2 xs)
cnfHelper2 (Dsj (x)) = Dsj x
cnfHelper2 (Cnj (x)) = Cnj x
--cnfHelper2 (Prop x) = Prop x
--cnfHelper2 (Neg (Prop x)) = Neg (Prop x)
--cnfHelper2 (Cnj x) = Cnj (map cnfHelper2 x)
--cnfHelper2 (Dsj [x]) = cnfHelper2 x
--cnfHelper2 (Cnj []) = Cnj []
--cnfHelper2 (Cnj [x]) = cnfHelper2 x
--cnfHelper2 (Cnj ((Cnj x):(Cnj y):z)) = cnfHelper2 (Cnj(x ++ y ++ z))
--cnfHelper2 (Dsj (x1:x2)) = distrOrAnd (cnfHelper2 x1) (cnfHelper2 (Dsj x2))
--cnfHelper2 (Dsj [(Cnj [x1:x2]):y]) = (Cnj ([Dsj([x1]++y)] ++ [Dsj(x2++y)]))
--cnfHelper2 (Dsj [a]) = cnfHelper2 a
--cnfHelper2 (Impl x y) = Impl x y
----cnfHelper2 (Equiv x y) = Equiv x y
--cnfHelper2 (Neg(Impl x y)) = Neg(Impl x y)
--cnfHelper2 (Equiv x y) = Equiv x y
--cnfHelper2 (Dsj []) = Dsj []
--cnfHelper2 x = x


--cnfHelper3 :: Form -> Form -> Form
--cnfHelper3 (Cnj [h]) y

form8 = Neg (Neg (Neg p))
form9 = Neg (Dsj [p, (Neg q)])
form10 = Neg (Cnj [(Neg p), (Neg q)])
form11 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
form12 = Cnj [Cnj [p ,q], Cnj [p, q]]
form13 = Dsj [Cnj [(Neg p) ,q], Cnj [p, (Neg q)]]
form14 = Dsj [p, Cnj [q,r]]
form15 = Dsj [p, q, r]

form16 = Neg (Dsj [p,q])
form17 = Dsj [Cnj [p,q], r]

getRandomNum :: IO Int
getRandomNum = getStdRandom (randomR (0,10))

genForm :: IO Form
genForm = genForm2 p

genForm2 :: Form -> IO Form
genForm2 x = do
  a <- getRandomNum
  y <- genForm
  case a of
    0 -> do
       return (Cnj[x,y])
    1 -> do
        return (Dsj[x,y])
    2 -> do
      return (p)
    3 -> do
      return (q)
    4 -> do
      return (Impl x y)
    5 -> do
      return (Equiv x y)
    _ -> do
      return (x)
