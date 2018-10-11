module Lecture5_ex5 where

import Lecture5_ex2

-- based on Remy's, Lukasś and Sebastiaans code
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

isMinimal :: Node -> IO Bool
isMinimal (s, n) = do
    let isUnique = uniqueSol (s, n)
    let hints = filledPositions s
    let subs = map (\x -> eraseN (s, n) x) hints
    if not isUnique then
        return False
    else
        return $ all (==False) $ map (\x -> uniqueSol x) subs

example7 :: Grid
example7 = [[0,0,0,7,6,0,2,0,9],
            [0,0,0,9,0,0,0,6,8],
            [0,0,0,0,0,0,1,0,0],
            [5,0,8,0,0,0,0,0,0],
            [0,0,4,0,0,0,0,0,0],
            [7,2,0,0,0,0,0,0,0],
            [0,0,0,0,0,6,5,0,0],
            [0,0,0,0,0,0,0,7,0],
            [0,0,0,1,0,0,0,0,4]]


example8 :: Grid
example8 =[[0,0,0,0,0,7,0,0,0],
            [0,0,0,0,0,4,0,0,0],
            [0,0,0,0,3,8,0,0,0],
            [0,1,3,0,0,0,0,0,9],
            [5,0,4,0,0,0,8,0,6],
            [7,0,2,0,0,0,0,0,0],
            [0,0,0,0,0,0,6,0,0],
            [0,0,0,0,0,0,0,1,0],
            [0,0,0,0,0,0,0,4,0]]

{--
ex4 :: Int -> IO ()
ex4 n = do
    r <- genRandomSudoku
    showNode r
    bs <- randomize [0..8]
    let x = eraseBs r (take n bs)
    showNode x
    temp <- genProblem x
    showNode temp
    ismin <- isMinimal temp
    print(ismin)
--}

-- Tries to find a suitable sudoku in 10 tries. Argument is number of
-- empty blocks.
-- Only differene with ex4 is that this version imports our new constraints.

ex5_21 :: Int -> IO ()
ex5_21 n = ex5_22 n False 0 emptyN emptyN emptyN

ex5_22 :: Int -> Bool -> Int -> Node -> Node -> Node -> IO ()
ex5_22 _ False 10 _ _ _ = do
  print("No sudoku found")
ex5_22 n False c _ _ _ = do
  print("Try")
  print(show c)
  print("/10")
  r <- genRandomSudoku
  bs <- randomize [0..8]
  let x = eraseBs r (take n bs)
  temp <- genProblem x
  ismin <- isMinimal temp
  ex5_22 n ismin (c+1) r x temp
ex5_22 n True c r x temp = do
  showNode r
  showNode x
  showNode temp
