module Lab6

where 

import Lecture6


-- Exercise 5
-- For Fermat's test, if a composite number n is not Carmichael,
-- then the probability that the test will detect compositeness is at least 1/2.
-- However, the test will fail all Carmichael numbers.
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | k <- [2..], prime (6*k+1), prime (12*k+1), prime (18*k+1) ]

recResult (x:xs) = do
    print(x)
    res <- primeTestsF 10 x
    print(res)
    recResult xs

result = recResult carmichael


-- Exercise 6
-- So the Miller-Rabin primality check gives all false for the generated carmichael list.
-- This is due to the correctness probability that is independent of the input (there are no "hard" inputs).
recResult6 (x:xs) = do
    print(x)
    res <- primeMR 10 x
    print(res)
    recResult6 xs

result6 = recResult6 carmichael

-- Exercise 6 2

recResult62 (x:xs) = do
    res <- primeMR 10 (2^x - 1)
    if(res) then print(2^x - 1) else return ()
    recResult62 xs

result62 = recResult62 primes