module Lecture5_ex4 where

import Lecture5_ex2

-- Exercise 4 (2 hours)
-- You can generate a sudoku with 3 and 4 empty blocks, 5 and up is not possible.
-- But only when the empty grids are not in the same row or collum grids.
-- Because when we have two empty grids in one direction, we can change
-- the row or collum of a number between the two grids.
-- Due to the extra constraints implemented in ex1 and ex2, this code generates a
-- NRC sudoku.
-- Works, but is very slow.
-- To run do ex4_21 3 or a amount of blocks you want to delete.

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

isMinimal :: Node -> IO Bool
isMinimal (s, n) = do
    let isUnique = uniqueSol (s, n)
    let hints = filledPositions s
    let subs = map (\x -> eraseN (s, n) x) hints
    if not isUnique then
        return False
    else
        return $ all (==False) $ map (\x -> uniqueSol x) subs

-- Tries to find a suitable sudoku in 10 tries. Argument is number of
-- empty blocks.

ex4_21 :: Int -> IO ()
ex4_21 n = ex4_22 n False 0 emptyN emptyN emptyN

ex4_22 :: Int -> Bool -> Int -> Node -> Node -> Node -> IO ()
ex4_22 _ False 10 _ _ _ = do
  print("No sudoku found")
ex4_22 n False c _ _ _ = do
  print("Try")
  print(show c)
  print("/10")
  r <- genRandomSudoku
  bs <- randomize [0..8]
  let x = eraseBs r (take n bs)
  temp <- genProblem x
  ismin <- isMinimal temp
  ex4_22 n ismin (c+1) r x temp
ex4_22 n True c r x temp = do
  showNode r
  showNode x
  showNode temp

