-- Tiling rectangles by minimal number of squares.
--
-- Inspired by David Radcliffe.
-- Author: Bertram Felgenhauer <int-e@gmx.de>
-- Date: 2012-11-12
-- src: http://int-e.eu/~bf3/squares/Young.hs

module Main (main) where

import Control.Monad

-- Young diagrams are described as lists of valleys.

-- Each valley has a left flank, a right flank,
-- and a lower bound for the size of the square that it contains.
data Valley = V !Int !Int !Int -- left, min square size, right
    deriving Show

infixr `cons`

-- Add a single square to a valley sequence.
extend :: [Valley] -> [[Valley]]
extend [] = []
extend (V a m b:vs) =
    [V (a-s) 1 s `cons` V s 1 (b-s) `cons` vs | s <- [m .. min a b]] ++
    [V a (min a b + 1) b `cons` v' | v' <- extend vs]

-- Prepend a valley to a valley sequence, coalescing flanks if one of the
-- adjacent flanks has length 0. The function `cons` maintains the invariant
-- that only the first valley of a valley sequence may have a zero length
-- flank, which must be a left flank.
cons :: Valley -> [Valley] -> [Valley]
V _ _ 0 `cons` [] = []
V a m 0 `cons` (V a' m' b' : vs) = V (a'+a) m' b' : vs
V a m b `cons` (V 0  m' b' : vs) = V a m (b+b') : vs
v       `cons` vs                = v : vs

-- Depth first search -- given an upper bound on the number of squares
-- required, find number of squares required for actual tilings.
fill :: [Valley] -> Int -> [Int]
fill [] bd = [0]
fill [V 0 _ _] bd = [0]
fill vs bd = do
    vs' <- extend vs
    guard (length vs' <= bd)
    i <- fill vs' (bd-1)
    return (i+1)

-- Look up minimum number of squares in `table`.
bound :: Int -> Int -> Int
bound n m | n < m     = table !! (m-1) !! (n-1)
          | otherwise = table !! (n-1) !! (m-1)

-- Table of minimum numbers of squares required to tile n*m rectangle.
table :: [[Int]]
table = map go [1..] where
    go n = [go' n m | m <- [1..n-1]] ++ [1]
    -- calculate entry for n*m rectangle
    go' n m =
        let -- use previous entries to obtain an upper bound
            bd = minimum $ [bound (n-k) m + bound k m | k <- [1..n-1]] ++
                           [bound n (m-k) + bound n k | k <- [1..m-1]]
            -- iterative flattening search - restart search with new bound
            -- whenever we obtain a better solution
            go'' bd = case fill [V n 1 m] (bd-1) of
                       [] -> bd
                       bd' : _ -> go'' bd'
        in  go'' bd

main :: IO ()
main = do
    mapM_ print $ take 40 table