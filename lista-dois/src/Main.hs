module Main (main) where
import Data.Bits (Bits(xor))

-- Arquivo para fazer alguns exercicios da lista 2 de programação funcional
-- verificar se palavra eh palindromo
palindromo :: Eq a => [a] -> Bool
palindromo x = x == reverse x

-- retornar o penultimo elemento de uma lista
penultimo :: [a] -> a
penultimo [] = error "lista vazia"
penultimo [x] = error "lista deve conter mais de um elemento"
penultimo [x : _ : []] = x
penultimo [_ : x : xs] = penultimo(x:xs)

-- cria lista de fatores de um numero
fatores :: Int -> [Int]
fatores n = [x | x <- [1 .. (n-1)], rem n x == 0]

-- cria lista de numeros perfeitos ate um n
perfeitos :: Int -> [Int]
perfeitos n = [x | x <- [1 .. n], sum (fatores x) == x]

produtoEscalar :: Num a => [a] -> [a] -> a
produtoEscalar v w = sum([x * y | (x, y) <- zip v w]) -- zip: igual um ziper, junta v[x] com w[y], iterando em conjunto

palindromoNum :: [Int] -> Bool
palindromoNum [] = True
palindromoNum [x] = True
palindromoNum x
    | head x == last x = palindromoNum (tail $ init x) -- tail $ init x, tira o ultimo elemento de x, e depois o primeiro
    | otherwise = False

ordenaListas :: (Num a, Ord a) => [[a]] -> [[a]]
ordenaListas [] = []
ordenaListas [x:xs] = ordenaListas menoresQueX ++ [x] ++ ordenaListas maioresQueX
    where
        menoresQueX = [a | a <- xs, length a <= length x]
        maioresQueX = [b | b <- xs, length b > length x]




coord :: [a] -> [a] -> [(a,a)]
--coord x y = [(i,j) | i <- x, j <- y]
coord x y = concat [[(i, j) |  j <- y] | i <- x]

mc91 :: Integral a => a -> a
mc91 n
    | n > 100 = n-10
    | otherwise = mc91 $ mc91 $ n + 11



main :: IO ()
main = do
    let s = "A(aaa)B(bbbb)[C(cccc),D(dddd),E(eeeee)]F(fff)"
    print(words s)
