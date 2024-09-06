module Main (main) where
import Data.List
import Text.Read

import Data.Monoid

-- 1
-- Deseja se somar a pontuacao de todas as notas, tal que:
-- se houver uma incidencia de cola, toda toda pontuacao deve ser descartada
-- monoid eh um semigrupo (associacao, conjunto) com um elemento neutro (mempty)
data Resultado = Pontuacao Int | Cola

instance Semigroup Resultado where
    (Pontuacao x) <> (Pontuacao y) = Pontuacao $ x + y
    _ <> _ = Cola

-- 2 Invariante: O set esta ordenado e nao tem elementos repetidos
data Set a = Set [a] deriving Eq

-- implementando show
instance Show a => Show (Set a) where
    show (Set xs) = "{" <> tail (init $ show xs) <> "}" -- por que usa isso ? 


-- implemente fromList
fromList :: Ord a => [a] -> Set a
fromList xs = Set $ (nub . sort) xs

-- implemente member, que retorna se um elemento pertence ao set
member :: Ord a => a -> Set a -> Bool
member e (Set xs) = e `elem` xs

-- implemente insert 
insert :: Ord a => a -> Set a -> Set a
insert e (Set xs) = fromList $ e:xs

-- implemente delete
delete :: Ord a => a -> Set a -> Set a
delete e (Set xs) = Set $ e `Data.List.delete` xs


-- 3 instancia de monoid Set
instance Ord a => Semigroup (Set a) where
    (Set as) <> (Set bs) = fromList $ as <> bs

instance Ord a => Monoid (Set a) where
    mempty = Set []

-- 4 Monoide de Dieta
data Dieta = Vegano | Vegetariano | Tradicional
instance Semigroup Dieta where
    Vegano <> Vegano = Vegano
    Vegano <> Vegetariano = Vegetariano
    Vegetariano <> Vegano = Vegetariano
    Vegetariano <> Vegetariano = Vegetariano
    _ <> _ = Tradicional

instance Monoid Dieta where
    mempty = Vegano

-- 5  Monoide de Lanche
data Lanche = Lanche (Set String) Int Dieta

instance Semigroup Lanche where
    (Lanche ing1 prec1 diet1) <> (Lanche ing2 prec2 diet2) = Lanche (ing1 <> ing2) (prec1 + prec2) (diet1 <> diet2)

instance Monoid Lanche where
    mempty = Lanche mempty 0 mempty


-- 6 definir functor da arvore binaria 
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
    fmap f (Leaf lv) = Leaf $ f lv
    fmap f (Node e nv d) = Node (fmap f e) (f nv) (fmap f d)

-- 7 defina arvore possui retorna True caso a seja um valor presente na árvore e False caso contrário
arvorePossui:: Ord a => a -> Tree a -> Bool
arvorePossui x (Leaf lv) = x == lv
arvorePossui x (Node e nv d) = x == nv || arvorePossui x e || arvorePossui x d

-- 8 defina contaLetras :: Tree String -> Tree Int e recebe uma árvore onde cada nó contém uma string e devolve uma nova árvore onde cada nó contém o comprimento das strings
contaLetras :: Tree String -> Tree Int
contaLetras = fmap length


-- 9 implementar foldable
instance Foldable Tree where
    foldMap f (Leaf lv) = f lv
    foldMap f (Node e nv d) = foldMap f e <> f nv <> foldMap f d

--10 implemente convertString2Int :: String -> Maybe Int
convertString2Int :: String -> Maybe Int
convertString2Int = readMaybe


-- 11 implemente nothingToZero :: Maybe Int -> Int
nothingToZero :: Maybe Int -> Int
nothingToZero Nothing = 0
nothingToZero (Just i) = i

-- 12 usando Data.Monoid para o monoide Sum, o foldable e as duas funcoes acima, defina frutasDaArvore :: Tree String -> Int, onde cada no tem uma string contendo quantas frutas tem
frutasDaArvore :: Tree String -> Int
frutasDaArvore = getSum . foldMap (Sum . nothingToZero . convertString2Int)


-- 13 Escreva Functor e Applicative de ZipList
-- funcao pura faz uma lista infinita de cópias do argumento
-- operador <*> aplica cada funcao argumento no valor correspondente da mesma posicao

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
    -- fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap f (Z xs) = Z $ fmap f xs

instance Applicative ZipList where
    -- pure :: a -> ZipList a 
    pure x = Z (repeat x) -- lista infinita de copias do argumento
    -- <*> :: f (a -> b) -> f a -> f b
    -- retorna lista de aplicacao de funcoes (g x) de cada elemento combinado das listas, encapsulado em Z
    (Z gs) <*> (Z xs) = Z [g x | (g,x) <- zip gs xs]

-- 15 defina Functor e Applicative de
newtype Identity a = Identity a

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    Identity g <*> x = fmap g x

data Pair a = Pair a a

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x

    (Pair x y) <*> (Pair z w) = Pair (x z) (y w)

-- 16 RLE
-- sequencia vazia eh End
-- nao vazia, Repeat
-- Repeat contem inteiro contendo numero de elemtnos repetidos do tipo a, seguido de outro Repeat ou End.
data RLE a = Repeat Int a (RLE a) | End deriving (Eq, Show)


main :: IO ()
main = undefined


