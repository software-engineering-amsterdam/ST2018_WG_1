module Lab5 where
import Data.List
import System.Random
import Test.QuickCheck

import Lecture5

-- Exercise 3: (2:00)
isMinimal :: Node -> IO Bool
isMinimal (s, n) = do
    let isUnique = uniqueSol (s, n)
    let hints = filledPositions s
    let subs = map (\x -> eraseN (s, n) x) hints
    if not isUnique then
        return False
    else
        return $ all (==False) $ map (\x -> uniqueSol x) subs

ex3 :: IO ()
ex3 = do
    [r] <- rsolveNs [emptyN]
    showNode r
    s <- genProblem r
    showNode s
    min <- isMinimal s
    if min then
        putStrLn "TRUE: The sudoku is minimal"
    else
        putStrLn "FALSE: The sudoku is NOT minimal"


-- Exercise 4:

eraseE :: Node -> [(Row, Column)] -> Node
eraseE n [] = n
eraseE n (x:xs) = eraseE (eraseN n x) xs

eraseB :: Node -> Integer -> Node
eraseB n b = eraseE n bl
    where
        r = blocks !! (floor $ fromIntegral b / 3)
        c = blocks !! (fromIntegral $ b `mod` 3)
        bl = [(x,y) | x <- r, y <- c]

eraseBs :: Node -> [Integer] -> Node
eraseBs n [] = n
eraseBs n (x:xs) = eraseBs (eraseB n x) xs


ex4 :: Int -> IO ()
ex4 n = do
    r <- genRandomSudoku
    showNode r
    bs <- randomize [0..8]
    let x = eraseBs r (take n bs)
    showNode x