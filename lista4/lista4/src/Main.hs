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
    (Pontuacao x) <> (Pontuacao y) = Pontuacao (x + y)
    _ <> _                         = Cola

instance Monoid Resultado where
    mempty = Pontuacao 0


-- 2
data Set a = Set [a] deriving Eq
-- Implemente a instancia de Show para o set, de modo que show set [1,2,3,4] apareça "{1,2,3,4}"
instance Show a => Show (Set a) where
    show (Set xs) = "{" <> tail (init $ show xs) <> "}"

-- implemente fromList
fromList :: Ord a => [a] -> Set a
fromList xs = Set $ nub $ sort xs

-- implemente member, que retorna se um elemento pertence ao set
member :: Ord a => a -> Set a -> Bool
member e (Set xs) = e `elem` xs


-- implemente insert 
insert :: Ord a => a -> Set a -> Set a
insert e (Set xs) = fromList $ e : xs

-- implemeente delete
delete :: Ord a => a -> Set a -> Set a
delete v (Set xs) = Set $ v `Data.List.delete` xs

-- instancia de monoid
instance Ord a => Semigroup (Set a) where
    (Set xs) <> (Set ys) = fromList $ xs <> ys -- como sabe que nao eh concat

instance Ord a => Monoid (Set a) where
    mempty = Set []

-- 4 Monoide de Dieta
data Dieta = Vegano | Vegetariano | Tradicional
data Lanche = Lanche (Set String) Int Dieta

instance Semigroup Dieta where
    Tradicional <> _ = Tradicional
    _ <> Tradicional = Tradicional
    Vegetariano <> _ = Vegetariano
    Vegano <> Vegetariano = Vegetariano
    Vegano <> Vegano = Vegano

instance Monoid Dieta where
    mempty = Vegano


-- 5  Monoide de Lanche
instance Semigroup Lanche where
    (Lanche i1 p1 d1) <> (Lanche i2 p2 d2) = Lanche (i1 <> i2) (p1 + p2) (d1 <> d2)

instance Monoid Lanche where
    mempty = Lanche mempty 0 mempty

-- 6 definir functor da arvore binaria 
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
    fmap f (Leaf x) = Leaf $ f x
    fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)

-- 7 defina arvore possui se True presente na arvore ou false se nao
arvorePossui :: Eq a => a -> Tree a -> Bool
arvorePossui v (Leaf x) = v == x
arvorePossui v (Node l x r) = arvorePossui v l || v == x || arvorePossui v r

-- 8 defina contaLetras :: Tree String -> Tree Int
contaLetras :: Tree String -> Tree Int
-- por que nao posso algo assim? contaLetras Tree = fmap length Tree 
contaLetras = fmap length

-- 9 implementar foldable
instance Foldable Tree where
    foldMap f (Leaf x) = f x
    foldMap f (Node l v r) = foldMap f l <> f v <> foldMap f r
    -- porque usa o <> em especifico, por que nao Node ......

--10 implemente convertString2Int :: String -> Maybe Int
convertString2Int :: String -> Maybe Int
convertString2Int = readMaybe

-- 11 implemente nothingToZero :: Maybe Int -> Int
nothingToZero :: Maybe Int -> Int
nothingToZero Nothing = 0
nothingToZero (Just a) = a

-- 12
frutasDaArvore :: Tree String -> Int
frutasDaArvore = getSum . foldMap (Sum . nothingToZero . convertString2Int ) 

-- 13




main :: IO ()
main = do
  let ls = [1,2,3,4,5]
  print (fromList ls)

