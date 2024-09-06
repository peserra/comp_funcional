{-# LANGUAGE InstanceSigs #-}
module Main (main) where
import Text.Read (Lexeme(Ident))


-- 1 Functor, Applicative e Monad do tipo Caixa
newtype Caixa a = Caixa a deriving(Eq, Show)

instance Functor Caixa where
    fmap :: (a -> b) -> Caixa a -> Caixa b
    fmap f (Caixa a) = Caixa $ f a

instance Applicative Caixa where
    pure :: a -> Caixa a
    pure = Caixa

    (<*>) :: Caixa (a -> b) -> Caixa a -> Caixa b
    (Caixa f) <*> (Caixa v) = Caixa $ f v

instance Monad Caixa where
    (>>=) :: Caixa a -> (a -> Caixa b) -> Caixa b
    (Caixa a) >>= f = f a

-- 3 Functor, Applicative e Monad para identity e pair
newtype Identity a = Identity a

instance Functor Identity where
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure :: a -> Identity a
    pure = Identity

    (<*>) :: Identity (a -> b) -> Identity a -> Identity b
    (Identity f) <*> (Identity v) = Identity $ f v

instance Monad Identity where
    return :: a -> Identity a
    return = pure

    (>>=) :: Identity a -> (a -> Identity b) -> Identity b
    (Identity v) >>= f = f v



data Pair a = Pair a a

instance Functor Pair where
    fmap :: (a -> b) -> Pair a -> Pair b
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure :: a -> Pair a
    pure x = Pair x x

    (<*>) :: Pair (a -> b) -> Pair a -> Pair b
    (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

instance Monad Pair where
    return :: a -> Pair a
    return = pure

    (>>=) :: Pair a -> (a -> Pair b) -> Pair b
    (Pair x y) >>= f = f y -- depende do contexto escolhe x ou y


-- 4 Functor, Applicative e Monad para tipo data Fantasma a = Fantasma

data Fantasma a = Fantasma

instance Functor Fantasma where
    fmap :: (a -> b) -> Fantasma a -> Fantasma b
    fmap _ _ = Fantasma

instance Applicative Fantasma where

    pure :: a -> Fantasma a
    pure _ = Fantasma

    (<*>) :: Fantasma (a -> b) -> Fantasma a -> Fantasma b
    _ <*> _ = Fantasma

instance Monad Fantasma where
    Fantasma >>= f = Fantasma
    -- poderia ser _ >>= _ = Fantasma ?


-- 5 Functor, Applicative e Monad de  data Duo a = Duo (Bool -> a)
data Duo a = Duo (Bool -> a)

instance Functor Duo where
    fmap :: (a -> b) -> Duo a -> Duo b
    fmap f (Duo g) = Duo $ f . g

instance Applicative Duo where
    pure x = Duo $ const x  -- porque? isso e uma funcao?

    (<*>) :: Duo (a -> b) -> Duo a -> Duo b
    (Duo f) <*> (Duo v) = Duo $ \x -> f x (v x)


--6 lib requisicoes http
-- qualquer erro em  apply e bind deve retornar erro
-- caso nao error e sim loading, loading

data Request a = Loading | Error | Success a

instance Functor Request where
    fmap :: (a -> b) -> Request a -> Request b
    fmap _ Error = Error
    fmap _ Loading = Loading
    fmap f (Success x) = Success $ f x

instance Applicative Request where
    pure :: a -> Request a
    pure = Success

    (<*>) :: Request (a -> b) -> Request a -> Request b
    _ <*> Error = Error
    Error <*> _ = Error
    _ <*> Loading = Loading
    Loading <*> _ = Loading
    (Success f) <*> (Success v) = Success $ f v

instance Monad Request where
    return :: a -> Request a
    return = pure

    (>>=) :: Request a -> (a -> Request b) -> Request b
    Error >>= _ = Error
    Loading >>= _ = Loading
    (Success v) >>= f = f v

--7 dado o tipo a seguir defina 

data Bolso a = Um a | Dois a a | Tres a a a

-- functor que aplica funcao em todas as posicoes
instance Functor Bolso where
    fmap :: (a -> b) -> Bolso a -> Bolso b
    fmap f (Um x) = Um (f x)
    fmap f (Dois x y) = Dois (f x) (f y)
    fmap f (Tres x y z) = Tres (f x) (f y) (f z)

-- instancia Eq para bolso, que compara apenas o valor mais a direita
instance Eq a => Eq (Bolso a) where
    (==) :: Eq a => Bolso a -> Bolso a -> Bool
    (Um a) == (Um b) = a == b
    (Dois _ a) == x = Um a == x
    (Tres _ _ a) == x = Um a == x
    x == y = y == x -- pra que eu preciso disso aqui?

-- instancia Applicative e Monad para bolso, sempre valor mais a direita e enviado para a funcao
instance Applicative Bolso where
    pure :: a -> Bolso a
    pure = Um

    (<*>) :: Bolso (a -> b) -> Bolso a -> Bolso b
    (Um f) <*> v = f <$> v
    (Dois _ f) <*> v = f <$> v
    (Tres _ _ f) <*> v = f <$> v

instance Monad Bolso where
    return :: a -> Bolso a
    return = pure

    (>>=) :: Bolso a -> (a -> Bolso b) -> Bolso b
    (Um x) >>= f = f x
    (Dois _ y) >>= f = f y
    (Tres _ _ z) >>= f = f z

-- 8 IMC
type Nome = String
type Altura = Double
type Peso = Double
type IMC = Double

calculate :: (Monad m, Fractional b) => (a, m b, m b) -> m b
calculate (_, peso, altura) = do
    p <- peso
    a <- altura
    return $ p / (a * a)

imc :: [(Maybe Nome, Maybe Peso, Maybe Altura)] -> [Maybe IMC]
imc = fmap calculate

-- 9 azul
azul :: Monad m => m (m a) -> m a
azul m = m >>= id

-- 10 amarelo 
amarelo :: Monad m => (a -> b) -> m a -> m b
amarelo = fmap

-- 11 vermelho
vermelho :: Monad m => (a -> b -> c) -> m a -> m b -> m c
vermelho f ma mb = pure f <*> ma <*> mb

-- 12 verde
verde :: Monad m => m a -> m (a -> b) -> m b
verde ma f = f <*> ma

-- 13 laranja
laranja :: Monad m => [m a] -> m [a]
laranja [] = return []
laranja (x:xs) = do
    x1 <- x
    xs1 <- laranja xs
    return $ x1:xs1

-- 14 roxo 
roxo :: Monad m => [a] -> (a -> m b) -> m [b]

main :: IO ()
main = do
  putStrLn "hello world"
