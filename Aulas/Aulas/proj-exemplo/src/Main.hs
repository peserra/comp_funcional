module Main where

import Test.QuickCheck
import qualified Data.List as L

and2 :: [Bool] -> Bool
and2 [] = True
and2 (x:xs) = x && and2 xs

and3 :: [Bool] -> Bool
and3 =  foldr (&&) True

concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 ([]:xss) = concat2 xss
concat2 ((x:xs):xss) = x : concat2 (xs : xss)

replicate2 :: Int -> a -> [a]
replicate2 n _ | n <= 0 = []
replicate2 n x =  x : replicate2 (n - 1) x

(!!!) :: [a] -> Int -> Either String a
(!!!) [] _ = Left "Erro"
(!!!) (x:_)  0 = Right x
(!!!) (_:xs) i = xs !!! (i - 1)


elem2 :: Eq a => a -> [a] -> Bool
elem2 _ [] = False
elem2 v (x:xs) = v == x || elem2 v xs


minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = 
    if x < mxs then x else mxs   
  where
    mxs = minimo xs

remove :: Eq a => a -> [a] -> [a]
remove v [] = []
remove v (x:xs) 
  | v == x = xs
  | otherwise = x : remove v xs

ssort :: Ord a => [a] -> [a]
ssort [] = []
ssort [x] = [x]
ssort xs =
  m : ssort rxs
  where
    m = minimo xs
    rxs = remove m xs



merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge x0@(x:xs) y0@(y:ys) 
  | x < y = x : merge xs y0
  | otherwise = y : merge x0 ys

split2 :: [a] -> ([a], [a])
split2 [] = ([], [])
split2 [x] = ([x], [])
split2 (x0:x1:xs) = (x0 : sxs0, x1 : sxs1)
  where
    (sxs0, sxs1) = split2 xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort m0) (msort m1)
  where
    (m0, m1) = split2 xs


qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
  where
    lhs = [e | e <- xs, e <= x]
    rhs = [e | e <- xs, e > x]

prop_idempotencia :: Ord a => [a] -> Bool
prop_idempotencia xs = msort (msort xs) == msort xs

prop_length :: Ord a => [a] -> Bool
prop_length xs = length (msort xs) == length xs

prop_ordem :: Ord a => [a] -> Bool
prop_ordem xs = msort xs == L.sort xs

-- >>> ['a', 'b', 'c'] !!! 0
-- 'a'





main :: IO ()
main = do
  putStrLn "hello world"
