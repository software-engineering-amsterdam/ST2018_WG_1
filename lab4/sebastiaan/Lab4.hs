module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

{--
 Exercise1:

 Question1:
 In chapter 4 they talk about sets and how they specify a set.
 In this particular example they gave the example of a specification as follows:

            {n^2| n ∈ {0, . . . , 999}}

 Now my question is does this specify a finite set from {0^2, ..., 999^2}?
 Or is it just to check wheter a number n is within a set {0, ..., 999}?


 Question2:
 Why is a set with a single element not the same as a that single element?


 Question3:
 Why is a set {a,b,b} the same as {a,b}? 

--}


{--
 Exercise2:


--}


{--
 Exercise3:

--}


{--
 Exercise4:

 Question1:
 The following definition was given for the domain of R:
 "The set dom (R) = {x | ∃y ( xRy )}, i.e., the set consisting of all first coordinates
 of pairs in R, is called the domain of R"

 Now they gave the example R = {(1, 4),(1, 5),(2, 5)} which, according to the book, has
 the domain "dom (R) = {1, 2}" but why is it not "dom (R) = {1, 1, 2}"?
 
 This might be resulting in the same answer as Question3 from Exercise1    

 Question2:
 So the integer partitions of 4 where 4 ∈ N+ are:

    A = [4], [1, 3], [2, 2], [1, 1, 2], [1, 1, 1, 1].

 Now they say that |A| == n in this case 4 but this results in 5? So do they skip one partition or something?

--}


{--
 Exercise5:
 

--}


type Rel a = [(a,a)]

symClos' :: Rel a -> Rel a -> Rel a
symClos' (Rel []) xs = xs
symClos' x xs = result where
    (a, b) = head x
    result = xs ++ [(a,b),(b,a)]

symClos :: Ord a => Rel a -> Rel a
symClos a = a (Rel [])

