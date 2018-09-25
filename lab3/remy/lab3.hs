module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3


--Exercise 1: (0:40)

contradiction :: Form -> Bool
contradiction f = not (satisfiable  f)


tautology :: Form -> Bool
tautology f = all (\v -> evl v f == True) (allVals f)

-- | logical entailment
-- a entails b
entails :: Form -> Form -> Bool
entails a b = all (\v -> evl v b == True) vTruth
    where
        vTruth = filter (\v -> evl v a == True) (allVals a)

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 = v1 == v2
    where
        v1 = filter (\v -> evl v f1) (allVals f1)
        v2 = filter (\v -> evl v f2) (allVals f2)


-- Exercise 2:
parseTest :: Form -> Bool
parseTest f = head (parse (show f)) == f

test1 = form1
test2 = form2
test3 = form3
test4 = Equiv form1 form2
test5 = Impl form1 form3
test6 = Equiv r (Neg r)
test7 = Dsj [form1, form2]
test8 = Cnj [form1, form2, form3]
test9 = r

allTests = [test1, test2, test3, test4, test5, test6, test7, test8, test9]

ex2Test = map parseTest allTests


-- Exercise 3
-- Find disjunctions and transform them into conjunctions.
formToCnf :: Form -> Form
formToCnf f = nnf $ arrowfree f




-- Exercise 4
-- TASKS:
-- + Choose random node types, choose between 0-5
-- - When conjunction or disjunction, make it possible to create multiple sub formulas
-- - Create random props (1-5)
-- - generate 2 random subforms for equiv and impl
-- 

randomBoundedInt :: Int -> Int -> IO Int
randomBoundedInt x y = randomRIO (x,y)


getNodeType :: Int -> String
getNodeType 0 = "Prop"
getNodeType 1 = "Neg"
getNodeType 2 = "Or"
getNodeType 3 = "And"
getNodeType 4 = "Impl"
getNodeType 5 = "Equiv"
getNodeType _ = error "Non existing node type"

randomNodeType :: IO String
randomNodeType = do
    x <- randomRIO (0,5)
    return (getNodeType x)


-- Generate a random form.
-- d is depth of the tree, if that is 0, than we create a prop instead of something else.
-- This way we won't go infinitely deep at random.
genForm :: Int -> Int -> IO Form
genForm 0 _ = do
    x <- randomBoundedInt 1 5
    return $ Prop x
genForm d n = do
    node    <- randomNodeType
    prop    <- randomBoundedInt 1 5
    f1      <- genForm (d-1) n
    f2      <- genForm (d-1) n
    x       <- randomBoundedInt 2 5
    fn      <- genForms x (d-1) n
    case node of
        "Prop"  -> return $ Prop prop
        "Neg"   -> return $ Neg f1
        "Or"    -> return $ Dsj fn
        "And"   -> return $ Cnj fn
        "Impl"  -> return $ Impl f1 f2
        "Equiv" -> return $ Equiv f1 f2
        _       -> error "Unknown node type"


genForms :: Int -> Int -> Int -> IO [Form]
genForms 0 _ _ = return []
genForms x d n = do
    f1 <- genForm d n
    fn <- genForms (x-1) d n
    return $ f1:fn

