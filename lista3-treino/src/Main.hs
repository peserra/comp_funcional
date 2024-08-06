module Main (main) where

-- 1 Duvida, como funciona o unwrap
data Nat = Zero | Succ Nat deriving Show
add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult m (Succ n) = add n (mult m n)

-- 2 
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show)

-- define se valor do tipo ordering é < = > q outro valor
--data Ordering = LT | EQ | GT
--compare :: Ord a => a -> a -> Ordering
-- usando isso, redefina para arvores de busca
occurs :: Ord a => a -> Tree a -> Bool
occurs a (Leaf b) = a == b
occurs a (Node l v r)
    | compare a v == EQ = True
    | compare a v == LT = occurs a l
    | otherwise = occurs a r

-- 3 Retorne lista ordenada com os elementos da árvore percorrida em ordem
-- esq meio dir
flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (Node l v r) = flatten l ++ [v] ++ flatten r

-- 4 considere a arvore binaria
-- balanceada: num folhas esquerda - direita no maximo 1
data TreeB a = LeafB a | NodeB (TreeB a) (TreeB a)

numLeaves :: TreeB a -> Int
numLeaves (LeafB _)   = 1
numLeaves (NodeB l r) = numLeaves l + numLeaves r

balanced :: TreeB a -> Bool
balanced (LeafB _)    = True
balanced (NodeB l r)  = abs (numLeaves l - numLeaves r) <= 1 && balanced l && balanced r

-- 5 converte lista nao vazia em arvore balanceada porque fica balanceada?
halve :: [a] -> ([a], [a])
halve xs = splitAt (length  xs `div` 2) xs

balance :: [a] -> TreeB a
balance [x] = LeafB x
balance xs = NodeB (balance ls) (balance rs)
    where (ls, rs) = halve xs

-- 6 usando a declaracao de tipo 
data Expr = Val Int | Add Expr Expr
-- defina a funcao de alta ordem tal que folde f g Expr substitui cada Val
-- em uma expressao pelo retorno de f e cada Add pelo retorno de g
-- f :: Int -> a
-- g :: a -> a -> a
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x)   = f x -- retorno de f
folde f g (Add x y) = g (folde f g x) (folde f g y) -- retorno de g

-- 7 usando folde, defina as funcoes:
-- avalia expressao para um valor inteiro (por que nao usar (Val 0)) ??
-- hoogle diz que id é util nos casos de alta ordem, porque?
eval :: Expr -> Int
eval = folde id (+)

-- calcula o numero de valores em uma expressao
size :: Expr -> Int
size = folde (\n -> 1) (+)

---------------------------------------------------------------------------------
--                                  INTERMEDIARIO                                   
--------------------------------------------------------------------------------
data List a = Nil | a :- List a
-- indica que o operador :- é infixo com precedencia 5, associativo a direita (simula cons)
infixr 5 :- 

-- ADT para modelar semaforo de transito
data Sem = Green | Yellow | Red deriving (Eq, Show)

-- funcao que conta o numero de semaforos com uma determinada cor numa lista
count :: Sem -> List Sem -> Int
count _ Nil = 0
count x (y :- ys) 
    | x == y    = 1 + count x ys
    | otherwise = count x ys

-- 8  escreva uma funcao next que recebe c :: Sem e devolve a cor que sucede c num semaforo
-- Ordem de sucessao: Green -> Yellow -> Red -> Green
next :: Sem -> Sem
next Green  = Yellow
next Yellow = Red
next Red    = Green

-- 9
-- automovel tem que atravessar lista de semaforos
-- leva uma unidade de tempo pra atravessar um semaforo
-- alem disso, demora uma unidade de tempo para o semaforo mudar de cor
-- Quanto tempo uma automovel leva para atravessar uma sequencia de semaforos ??

-- condicoes: 
--  - [Green, Yellow, Red]. Cada atravessada leva uma unidade de tempo
--  - se semaforo agora é red, tem que esperar mais uma unidade de tempo
--  - resta so atravessar o Green, com mais uma unidade de tempo
--  timeList recebe xs e devolve o tempo que um automovel leva para atravessar

mudaSemaforo :: List Sem -> List Sem
mudaSemaforo Nil = Nil
mudaSemaforo (x:-xs) = next x :- mudaSemaforo xs

timeList :: List Sem -> Int
timeList Nil = 0
timelist (x:-xs)
    | x == Red      = 2 + timelist (mudaSemaforo xs)
    | otherwise     = 1 + timelist (mudaSemaforo xs)

main :: IO ()
main = do
    undefined
