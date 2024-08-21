module Main where

import Data.Map hiding (insert)

-- type String = [Char]

newtype Texto = T String

data Texto2 = T2 String 

-- >>> :t T2
-- T2 :: String -> Texto2

data Arvore a = 
    No {
      esquerda :: Arvore a,
      valor :: a,
      direita :: Arvore a 
    }
  | Vazia
  deriving (Show, Read)

data Fuzzy = Verd | Falso | Talvez MeuFloat deriving Show

-- >>> show $ Talvez 3.14
-- "Talvez 3.14"



-- >>> show (No (No Vazia 5 Vazia) 2 Vazia)
-- "No {esquerda = No {esq = Vazia, valor = 5, dir = Vazia}, valor = 2, direita = Vazia}"






-- instance Show (Arvore a) where
--   show Vazia = "Vazia"
--   show (No e _ d) = "No (" ++ show e ++ ") VALOR (" ++ show d ++ ")"


newtype MeuFloat = MF Float

instance Show MeuFloat where
  show (MF x) = "Blaa" ++ show x

instance Eq2 MeuFloat where
  (MF x) === (MF y) = abs (x - y) < 0.00001

-- >>> MF 5 
-- MF 5.0

class Eq2 a where
  (===) :: a -> a -> Bool
  (/==) :: a -> a -> Bool

  x /== y = not (x === y)

instance Eq2 Int where
  x === y = x == y















-- >>> No (No Vazia 2 Vazia) 3 (No Vazia 5 Vazia)
-- No (No (Vazia) VALOR (Vazia)) VALOR (No (Vazia) VALOR (Vazia))


-- >>> show $ No Vazia True Vazia
-- "No {esquerda = Vazia, valor = True, direita = Vazia}"




insert :: Ord a => a -> Arvore a -> Arvore a
insert x Vazia = No Vazia x Vazia
insert x (No e v d) 
  | x == v = No e v d
  | x < v = No (insert x e) v d
  | otherwise = No e v (insert x d)

-- foldr insert Vazia [1, 2, 3, 10]

-- >>> :t esquerda
-- esquerda :: Arvore a -> Arvore a

esq :: Arvore a -> Arvore a
esq (No e _ _) = e

dir :: Arvore a -> Arvore a
dir (No _ _ d) = d

val :: Arvore a -> a
val (No _ v _) = v




contem :: Eq a => a -> Arvore a -> Bool
contem _ Vazia = False
contem x (No e v d) = 
  v == x || contem x e || contem x d

contemB :: Ord a => a -> Arvore a -> Bool
contemB _ Vazia = False
contemB x (No e v d)
  | x == v = True
  | x < v  = contemB x e
  | otherwise = contemB x d

--data Maybe a = Nothing | Just a 

--data Either a b = Left a | Right b

-- >>> :t Just 3
-- Just 3 :: Num a => Maybe a

safeDiv :: Int -> Int -> Either String Int
safeDiv _ 0 = Left "nao pode dividir por 0"
safeDiv x y = Right $ div x y

-- data Void
-- data () = ()

data X = A Int Char | B Bool




-- >>> :t True
-- True :: Bool

main :: IO ()
main = do
  putStrLn "hello wo" 
