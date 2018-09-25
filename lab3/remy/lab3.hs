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





-- Exercise 4: (2:00)
-- Node rules:
-- - Prop:  Followed only by a Prop/Int.
-- - Neg:   Followed by a subform.
-- - Dsj:   Followed by a list of subforms.
-- - Cnj:   Followed by a list of subforms.
-- - Impl:  Followed by 2 separate subforms.
-- - Equiv: Followed by 2 separate subforms.
--
-- There are no preconditions for the generator.
-- There are postconditions though:
-- - The output must be a valid formula.
--   This is already checked in the generator, because the output
--   has to be of type IO Form.
-- - Furthermore could the generator fail if we do not create any Props.
--   This is done by placing Props at every leaf in the generator.
--   Also we defined a max depth, and at max depth we automatically place a Prop as leaf.

getNodeType :: Int -> String
getNodeType 0 = "Prop"
getNodeType 1 = "Neg"
getNodeType 2 = "Dsj"
getNodeType 3 = "Cnj"
getNodeType 4 = "Impl"
getNodeType 5 = "Equiv"
getNodeType _ = error "Non existing node type"

randomNodeType :: [Int] -> IO String
randomNodeType l = do
    x <- randomRIO (0, (length l)-1)
    return $ getNodeType $ l !! x

-- Generate a random form.
-- d is depth of the tree, if that is 0, than we create a prop instead of something else.
-- This way we won't go infinitely deep at random.
-- nt is a list of nodeTypes that are allowed to be used in the (sub)form.
genForm :: Int -> [Int] -> IO Form
genForm 0 _ = do
    x <- randomRIO (1,5)
    return $ Prop x
genForm d nt = do
    node        <- randomNodeType nt
    prop        <- randomRIO (1,5)
    f1          <- genForm (d-1) [0,1,2,3,4,5]
    f2          <- genForm (d-1) [0,1,2,3,4,5]
    x           <- randomRIO (2,5)
    fn          <- genForms x (d-1) node
    case node of
        "Prop"  -> return $ Prop prop
        "Neg"   -> return $ Neg f1
        "Dsj"   -> return $ Dsj fn
        "Cnj"   -> return $ Cnj fn
        "Impl"  -> return $ Impl f1 f2
        "Equiv" -> return $ Equiv f1 f2
        _       -> error "Unknown node type"


-- This generates a list of forms for conjunctions and disjunctions.
genForms :: Int -> Int -> String -> IO [Form]
genForms 0 _ _ = return []
genForms x d n = do
    f1 <- genForm d $ filter (\x -> getNodeType x /= n) [0,1,2,3,4,5]
    fn <- genForms (x-1) d n
    return $ f1:fn

-- This function executes a function on a random generated formula.
-- This works with the testParse function
-- Also a depth should be given, so that we do not necessarily generate huge formulas.
testParseRandom :: [Form] -> (Form -> Bool) -> IO ()
testParseRandom [] _ = putStrLn "Passed test!"
testParseRandom (f:fn) p = do
    if (p f) then
        do
            testParseRandom fn p
    else
        putStrLn $ "Failed test:\n" ++ (show f)

genNForms :: Int -> Int -> IO [Form]
genNForms 0 _ = return []
genNForms n d = do
    f   <- genForm d [0,1,2,3,4,5]
    fn  <- genNForms (n-1) d
    return $ f:fn

-- This function executes a function N times on random generated formulas.
testParseRandomN :: Int -> Int -> (Form -> Bool) -> IO ()
testParseRandomN 0 _ _ = putStrLn "Testing 0 times is not very useful, is it?"
testParseRandomN n d p = do
    ranForms <- genNForms n d
    putStrLn $ "--- Start " ++ show n ++ " tests!"
    testParseRandom ranForms p

-- Test properties
randTautology :: IO ()
randTautology = testParseRandomN 100 3 tautology

randContradiction :: IO ()
randContradiction = testParseRandomN 100 3 contradiction

randEquiv1, randEquiv2 :: IO ()
randEquiv1 = testParseRandomN 100 3 (\x -> equiv x x)
randEquiv2 = testParseRandomN 100 3 (\x -> equiv x $ Neg x)