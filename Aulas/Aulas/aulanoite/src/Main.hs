module Main where

import Debug.Trace
import Data.List 
import Test.QuickCheck
import GHC.IO.Buffer (BufferState(ReadBuffer))
import Data.Bits (Bits(xor))
import Data.Bifoldable (bifoldl1)
import Text.Read

-- Decide se todos os valores lógicos de uma lista são True
and2 :: [Bool] -> Bool
and2 [] = True
and2 (x:xs) = x && and2 xs

and3 :: [Bool] -> Bool
and3 xs = foldr (&&) True xs

-- >>> :t foldr
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-- Concatena uma lista de listas
concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 ([]:xss) = concat2 xss
concat2 ((x:xs):xss) = x : concat2 (xs : xss)


-- [[1,2],[3,4]] 
-- x 1 xs [2] xss [[3,4]] 
-- [[3,4]]


-- Produz uma lista com n valores idênticos
replicate2 :: Int -> a -> [a]
replicate2 n _ | n <= 0 = []
replicate2 n x = x : replicate2 (n - 1) x

-- Seleciona o enésimo elemento de uma lista
(!!!) :: [a] -> Int -> a
[] !!! _ = error "nao pode"
(x:_) !!! 0 = x
(_:xs) !!! n = xs !!! (n - 1)

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = if x < mxs then x else mxs
  where
    mxs = minimo xs

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove v (x:xs)
  | v == x = xs
  | otherwise = x : remove v xs

ssort :: Ord a => [a] -> [a]
ssort [] = []
ssort xs = m : ssort xs'
  where
    m = minimo xs
    xs' = remove m xs


prop_tamanho :: Ord a => ([a] -> [a]) -> [a] -> Bool
prop_tamanho f xs = length (f xs) == length xs

prop_modelo :: ([Int] -> [Int]) -> [Int] -> Bool
prop_modelo f xs = f xs == sort xs

prop_idem :: ([Int] -> [Int]) -> [Int] -> Bool
prop_idem f xs = f (f xs) == f xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge x0@(x : xs) y0@(y:ys)
  | x < y = x : merge xs y0
  | otherwise = y : merge x0 ys

split2 :: [a] -> ([a], [a])
split2 = split2' [] []
  where
    split2' ls rs [] = (ls, rs)
    split2' ls rs (x:xs) = (x : ls1, rs1)
      where
        (rs1, ls1) = split2' rs ls xs


msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort lxs) (msort rxs)
  where
    (lxs, rxs) = split2 xs


data Nota = Do | Re | Mi | Fa | Sol | La | Si deriving (Show, Enum)
data ModoGrego =
    Jonio
  | Dorico
  | Frigio
  | Lidio
  | Mixolidio
  | Eolio
  | Locrio deriving (Show, Enum)

-- >>> fromEnum Eolio
-- 5

-- >>> [Do ..]
-- [Do,Re,Mi,Fa,Sol,La,Si]

-- >>> toEnum 7 :: Nota
-- toEnum{Nota}: tag (7) is outside of enumeration's range (0,6)


newtype MProd = MK Int deriving Show

newtype MSoma = MSoma Int deriving Show
instance Semigroup MSoma where
  (MSoma x) <> (MSoma y) = MSoma $ x + y
instance Monoid MSoma where
    mempty = MSoma 0

instance Semigroup MProd where
  (MK x) <> (MK y) = MK $ x * y


instance Monoid MProd where
  mempty = MK 1


instance Semigroup Int where
  (<>) = (+)

instance Monoid Int where
  mempty = 0



monoConcat :: Monoid a => [a] -> a
monoConcat [] = mempty
monoConcat (x:xs) = x <> monoConcat xs



notaDeInt :: Int -> Nota
notaDeInt n = toEnum $ n `mod` 7

proxNota :: Nota -> Nota
proxNota n = notaDeInt $ fromEnum n + 1

notas2 :: [Nota]
notas2 = map notaDeInt [0..]

notas :: [Nota]
notas = go Do
  where
    go ini = ini : go (proxNota ini)

-- >>> take 10 notas 
-- [Do,Re,Mi,Fa,Sol,La,Si,Do,Re,Mi]

gerarModo :: Int -> ModoGrego -> [Nota]
gerarModo n m = take n (drop (fromEnum m) notas)

-- >>> gerarModo 12 Dorico
-- [Re,Mi,Fa,Sol,La,Si,Do,Re,Mi,Fa,Sol,La]


newtype Soma = Soma Int deriving Show
newtype Prod = Prod Int deriving Show

getSoma :: Soma -> Int
getSoma (Soma x) = x

getProd :: Prod -> Int
getProd (Prod x) = x

instance Semigroup Soma where
  (Soma x) <> (Soma y) = Soma $ x + y

instance Semigroup Prod where
  (Prod x) <> (Prod y) = Prod $ x * y

instance Monoid Soma where
  mempty = Soma 0

instance Monoid Prod where
  mempty = Prod 1

-- >>> foldl (<>) mempty (map Soma [2, 1, 2, 3])
-- Soma 8

-- >>> foldMap2 Prod [1..10]
-- Prod 3628800

data Arv a = Folha a | No (Arv a) a (Arv a) deriving Show

instance Functor Arv where
  fmap f (Folha x) = Folha $ f x
  fmap f (No e x d) = No (fmap f e) (f x) (fmap f d)

instance Foldable Arv where
  foldMap f (Folha x) = f x
  foldMap f (No e x d) =
    foldMap f e <>
    f x <>
    foldMap f d

--tam arv = foldMap (const (Soma 1)) arv


-- >>> foldMap Soma [1, 2, 3, 4]
-- Soma 10

-- >>> maximum (No (Folha 5) 3 (Folha 7))
-- 7

somaCoisas :: Foldable t => t Int -> Int
somaCoisas xs = getSoma $ foldMap Soma xs


-- >>> a $ b

-- >>> f <$> xs 

-- >>> somaCoisas [1..10]
-- 55

-- data Maybe a = Nothing | Just a

-- fmap :: Functor t => (a -> b) -> t a -> t b

-- >>> fmap even (Nothing :: Maybe Int)
-- Nothing

-- >>> even <$> []
-- []

-- >>> even <$> Folha 5
-- Folha False

-- $      <$>   <> 
-- apply  fmap  mconcat 


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x


-- safeDiv :: Int -> Int-> Maybe Int 
-- safeDiv _ 0 = Nothing
-- safeDiv x y = Just $ x `div` y

-- >>> foo (30 `safeDiv` 5) 4
-- Just 10

-- >>> fmap (+4) (30 `safeDiv` 5)
-- Just 10

-- >>> (+4) <$> (30 `safeDiv` 5)
-- Just 10

-- 30 / 5 + 4


data SafeNum a = NaN | NegInf| PosInf | SafeNum a deriving Show

instance Functor SafeNum where
  fmap f (SafeNum x) = SafeNum $ f x
  fmap _ x = boxedCoerce x
  
-- >>> fmap (+3) (SafeNum 10)
-- SafeNum 13


safeAdd :: Int -> Int -> SafeNum Int
safeAdd x y
  | signum x /= signum y = SafeNum z
  | signum z /= signum x = if signum x > 0
                           then PosInf
                           else NegInf
  | otherwise = SafeNum z
  where z = x + y

safeDiv :: Int -> Int -> SafeNum Int
safeDiv 0 0 = NaN
safeDiv x 0
  | x > 0     = PosInf
  | otherwise = NegInf
safeDiv x y = SafeNum $ x `div` y



-- Devolve (x / y) + (y / x)
-- Versão inicial
f0 :: Int -> Int -> SafeNum Int
f0 x y
  | isSafe xy && isSafe yx = safeAdd (unbox xy) (unbox yx) -- SafeNum Int
  | (not.isSafe) xy = xy
  | otherwise = yx -- (not.isSafe) yx
  where
    xy = safeDiv x y -- SafeNum Int
    yx = safeDiv y x -- SafeNum Int
    unbox (SafeNum x) = x
    isSafe (SafeNum _) = True
    isSafe _            = False


boxedCoerce :: SafeNum a -> SafeNum b
boxedCoerce NaN = NaN
boxedCoerce NegInf = NegInf
boxedCoerce PosInf = PosInf
boxedCoerce _ = error "Não deveria ser usado para valores safe"

flatten :: SafeNum (SafeNum a) -> SafeNum a
flatten (SafeNum sn) = sn
flatten v = boxedCoerce v

dec :: Int -> Maybe Int
dec x | x <= 0    = Nothing
      | otherwise = Just (x - 1)

-- >>> traverse dec [1, 2, 0]
-- Nothing

soma :: [Int] -> Int 
soma [] = 0
soma (x:xs) = 
  trace (show sp) $
  sp
  where 
    sp = x + soma xs





-- Versão com functors
-- Devolve (x / y) + (y / x)
f1 :: Int -> Int -> SafeNum Int
f1 x y =
  let xy = safeDiv x y
      yx = safeDiv y x
      -- :: SafeNum (Int -> SafeNum Int)
      safeAddXY = fmap safeAdd xy
      -- :: SafeNum (SafeNum (SafeNum Int))
      safeXYPlusYX = fmap (`fmap` yx) safeAddXY
  in
    (flatten.flatten) safeXYPlusYX






-- >>> fmap even (Nothing :: Maybe Int)
-- Maybe Bool


-- data Resultado = Pontuacao Int | Cola

-- instance Semigroup Resultado where
--   (Pontuacao x) <> (Pontuacao y) = Pontuacao $ x + y
--   _  <> _ = Cola
  
-- instance Monoid Resultado where
--   mempty = Pontuacao 0

-- -- >>> Pontuacao 0 <> Pontuacao 398  

-- data Set a = Set [a] deriving Eq

-- --  Set [1,2,4] → "{1,2,4}"

-- instance Show a => Show (Set a) where
--   -- show (Set xs) = '{' : intercalate "," (show <$> xs) <> "}"
--   show (Set xs) = init ("{" ++ tail sl) ++ "}"
--     where
--       sl = show xs
-- -- >>> Set [1, 2, 5]
-- -- {1,2,5}

-- fromList :: Ord a => [a] -> Set a
-- fromList = Set . nub . sort

-- member :: Ord a => a -> Set a -> Bool 
-- member _ (Set []) = False
-- member v (Set (x:xs)) = v == x || member v (Set xs)

-- insert :: Ord a => a -> Set a -> Set a
-- insert v (Set xs) = fromList $ v : xs

-- delete :: Ord a => a -> Set a -> Set a
-- delete v (Set xs) = Set $ Data.List.delete v xs

-- instance Ord a => Semigroup (Set a) where
--   (Set xs) <> (Set ys) = fromList $ xs <> ys

-- instance Ord a => Monoid (Set a) where
--   mempty = Set []


-- data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

-- instance Functor Tree where
--   fmap f (Leaf x) = Leaf $ f x
--   fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

-- contaLetras :: Tree String -> Tree Int
-- contaLetras = fmap length

-- instance Foldable Tree where
--   foldMap f (Leaf v) = f v
--   foldMap f (Node l v r) = foldMap f l <> f v <> foldMap f r

-- convertString2Int :: String -> Maybe Int
-- convertString2Int = readMaybe

-- nothingToZero :: Maybe Int -> Int
-- nothingToZero Nothing = 0
-- nothingToZero (Just x) = x

-- frutasDaArvore :: Tree String -> Int
-- frutasDaArvore = sum . fmap (nothingToZero . convertString2Int)


-- comp :: String -> Int
-- comp xs = length xs

-- >>>:t readLn
-- readLn :: Read a => IO a

-- data Expr = Val Int
--           | Add Expr Expr
--           | Sub Expr Expr
--           | Mul Expr Expr
--           | Div Expr Expr


-- eval :: Expr -> Maybe Int
-- eval (Val n)   = Just n
-- eval (Div x y) = case eval x of
--                     Nothing -> Nothing
--                     Just n  -> case eval y of
--                                   Nothing -> Nothing
--                                   Just m  -> maybeDiv n m

-- instance Functor Maybe where
--   fmap f Nothing = Nothing
--   fmap f (Just x) = Just $ f x

-- instance Monad Maybe where
--    Nothing  >>= _ = Nothing
--    (Just x) >>= f = f x


data Resultado = Pontuacao Int | Cola

instance Semigroup Resultado where
  (Pontuacao x) <> (Pontuacao y) = Pontuacao $ x + y 
  _ <> _  = Cola 
  
instance Monoid Resultado where
  mempty = Pontuacao 0


data Set a = Set [a] deriving Eq

instance Show a => Show (Set a) where
  show (Set xs) = "{" <> init (tail sl) <> "}"
    where 
      sl = show xs

fromList :: Ord a => [a] -> Set a
fromList xs = Set $ nub (sort xs)

member :: Ord a => a -> Set a -> Bool
member v (Set xs) = v `elem` xs

insert :: Ord a => a -> Set a -> Set a
insert v (Set xs) =  fromList $ v : xs

delete :: Ord a => a -> Set a -> Set a
delete v (Set xs) = Set $ v `Data.List.delete` xs

instance Semigroup (Set a) where
  (Set xs) <> (Set ys) = fromList $ xs <> ys

instance Monoid (Set a) where
  mempty = Set []

data Dieta = Vegano | Vegetariano | Tradicional
data Lanche = Lanche (Set String) Int Dieta

instance Semigroup Dieta where
  Vegano <> Vegano = Vegano
  Vegetariano <> Vegano = Vegetariano
  Vegano <> Vegetariano = Vegetariano
  Vegetariano <> Vegetariano = Vegetariano
  _ <> _ = Tradicional

instance Monoid Dieta where
  mempty = Vegano  

instance Semigroup Lanche where
  (Lanche ing0 p0 d0) <> (Lanche ing1 p1 d1) =
    Lanche (ing0 <> ing1) (p0 + p1) (d0 <> d1)

instance Monoid Lanche where
  mempty = Lanche mempty 0 mempty


data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node e v d) = Node (fmap f e) (f v) (fmap f d)

arvorePossui :: Eq t => t -> Tree t -> Bool
arvorePossui v (Leaf x) = v == x
arvorePossui v (Node e x d) = v == x || arvorePossui v e || arvorePossui v d

contaLetras :: Tree String -> Tree Int
contaLetras t = fmap length t


main :: IO ()
main = do
  linha <- readLn :: IO String
  print "banana"
  quickCheck (prop_tamanho ssort :: [Int] -> Bool)
  quickCheck (prop_tamanho ssort :: [Char] -> Bool)
  quickCheck $ prop_modelo ssort
  quickCheck $ prop_idem ssort
  quickCheck (prop_tamanho msort :: [Int] -> Bool)
  quickCheck (prop_tamanho msort :: [Char] -> Bool)
  quickCheck $ prop_modelo msort
  quickCheck $ prop_idem msort

