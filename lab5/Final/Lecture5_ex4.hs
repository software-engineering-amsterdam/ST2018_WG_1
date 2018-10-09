module Lecture5_ex4 where

import Lecture5

-- Exercise 4 (2 hours)
-- You can generate a sudoku with 3 empty grids, but not with 4 or 5.
-- But only when the empty grids are not in the same row or collum grids.
-- Because when we have two empty grids in one direction, we can change
-- the row or collum of a number between the two grids. This can
-- be checked by removing grids, them minimizing the sudoku. Then we can
-- check if there is a signle solution by running the uniqueSol function.

checkMinimal (sudoku, con) = do
    let hintPositions = filledPositions sudoku
    let subSudokus = map (\x -> eraseN (sudoku, con) x) hintPositions
    return $ all (==False) $ map (\x -> uniqueSol x ) subSudokus

eraseE :: Node -> [(Row, Column)] -> Node
eraseE n [] = n
eraseE n (x:xs) = eraseE (eraseN n x) xs

-- Replaces grid b with empty grid.
eraseB :: Node -> Integer -> Node
eraseB n b = eraseE n bl
    where
        r = blocks !! (floor $ fromIntegral b / 3)
        c = blocks !! (fromIntegral $ b `mod` 3)
        bl = [(x,y) | x <- r, y <- c]

eraseBs :: Node -> [Integer] -> Node
eraseBs n [] = n
eraseBs n (x:xs) = eraseBs (eraseB n x) xs

-- This function removes the grids listed in x and then minimalize
-- the sudoku. After that we check that this is the minimal solution.
test4 :: [Integer] -> IO ()
test4 x = do
    s <- genRandomSudoku
    showNode s
    let sx = eraseBs s x
    showNode sx
    sy <- genProblem sx
    showNode sy
    ismin <- checkMinimal sy
    print(ismin)
