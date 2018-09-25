module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- Exercise 1 (1 hour)
-- To test are definitions we used two forms per definiotion.
-- The first test per definition should be true and the second
-- should be false. Thus test1a, test1c, test1e and test1g should
-- be true and test1b, test1d, test1f and test1h should be fasle.
form1a = Cnj [p, Neg p]
form1b = Dsj [p, Neg p]

contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

test1a = contradiction form1a
test1b = contradiction form1b

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

test1c = tautology form1b
test1d = tautology form1a

-- | logical entailment
-- The result answers the quistion if f2 logically entails f1.
entails :: Form -> Form -> Bool
entails f1 f2 =  all (\ v -> evl v f1) (filter (\ v -> evl v f2) (allVals f2))

test1e = entails form1 form2
test1f = entails form2 form1

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 = (filter (\ v -> evl v f1) (allVals f1)) == (filter (\ v -> evl v f2) (allVals f2))

test1g = equiv form1 form1
test1h = equiv form1 form2

-- Exercise 2

-- Exercise 3 (1.5 hour)
-- When translating a form to CNF we need to make the form arrowfree,
-- then make it to negation normal form and then distribute
-- disjuntions inward according to:https://en.wikipedia.org/wiki/Conjunctive_normal_form
-- The arrowfree and nnf are given so we only need to implement
-- the function cnf which distribute the disjunctions inwards.
toCNF :: Form -> Form
toCNF f = cnf (nnf (arrowfree f))

-- This function distribute the disjunctions inwards by looking at
-- every disjunction and replace "P or (Q and R)" with
-- "(P or Q) and (P or R)"
cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Neg (Prop x)) = Neg (Prop x)
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj [Cnj [f1, f2], x]) = Cnj [cnf (Dsj [x, f1]), cnf (Dsj [x, f2])]
cnf (Dsj [x, Cnj [f1, f2]]) = Cnj [cnf (Dsj [x, f1]), cnf (Dsj [x, f2])]
cnf (Dsj fs) = Dsj (map cnf fs)

-- Here are some text forms from the wiki where are also the
-- expected results.
s = Prop 4
form3a = Dsj [p, Cnj [q,r]]
form3b = Dsj [p, q]
form3c = Dsj [Cnj [p, q], r]
form3d = Cnj [p, Dsj [q, Cnj [r, s]]]

test3a = toCNF form3a
test3b = toCNF form3b
test3c = toCNF form3c
test3d = toCNF form3d


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

randCNF :: IO ()
randCNF = testParseRandomN 100 3 (\x -> equiv (toCNF x) x)

-- randParse :: IO ()
-- randParse = testParseRandomN 100 3 testParse