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
equiv f1 f2 = (filter (\ v -> evl v f1) (allVals f1)) ==
(filter (\ v -> evl v f2) (allVals f2))

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
