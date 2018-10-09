module Lecture5_ex4 where

import Lecture5_ex2

-- based on Remy's and Sebastiaans code
-- Due to the extra constraints implemented in ex1 and 2, this code generates a
-- NRC sudoku.
-- Works, but is very slow. 

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
    temp <- genProblem x
    showNode temp
