-- 2024-09-09 - Emilio Francesquini <e.francesquini@ufabc.edu.br>
-- Universidade Federal do ABC
-- CC-BY-SA 4.0

module Main (main) where
import Data.List (nub)
import Data.Maybe (fromJust)
import System.Random

-- A definição de State
newtype State s a = State (s -> (a, s))

-- Definições de Functor, Applicative e Monad para State
instance Functor (State s) where
  -- fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State (\s -> let (a, s') = g s
                                  in  (f a, s'))

instance Applicative (State s) where
  pure x = State (\s -> (x, s))
  -- (<*>) :: State s (a -> b) -> State s a -> State s b
  sab <*> sa = State (\s -> let (f, s1) = runState sab s
                                (a, s2) = runState sa  s1
                            in  (f a, s2))

instance Monad (State s) where
  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  sa >>= f = State (\s -> let (a, s1) = runState sa s
                              sb      = f a
                          in  runState sb s1)



-- Executa um State dado um contexto inicial
runState :: State s a -> s -> (a, s)
runState (State f) = f

-- State que, quando executado, devolve o seu estado interno
get :: State s s
get = State (\s -> (s, s))

-- State que, quando executado, aplica a função dada como parâmetro
-- ao estado  interno e devolve o valor calculado.
gets :: (s -> a) -> State s a
gets f =
  get >>= return . f

-- Dado um novo coontexto, coloca-o dentro do State
put :: s -> State s ()
put x = State (const ((), x))

-- Dada uma função, aplica-a ao contexto atual e coloca o resultaado
-- como novo contexto do State
modify :: (s -> s) -> State s ()
modify f = do
  s <- get
  put $ f s
  return ()

-- Semelhante ao runState mas devolve apenas o contexto final
exec :: State s a -> s -> s
exec st = snd . runState st

-- Semelhante ao runState mas devolve apenas o valor final
eval :: State s a -> s -> a
eval st = fst . runState st


-- =============================================================================
-- =============================================================================
-- =============================================================================


-- Vamos agora usar a State Monad para fazer um jogo da velha bem
-- simples

-- Coordenada de uma casa do tabuleiro
type Coord = (Int, Int)

-- Dicionário simples de pares chave/valor
type Dict k v = [(k, v)]

-- Consulta e devolve, caso exista, a entrada do dicionário cuja chave
-- corresponde à chave fornecida
dictGet :: Eq k => k -> Dict k v -> Maybe v
dictGet _ [] = Nothing
dictGet k ((k0,v0) : kvs)
  | k == k0 = Just v0
  | otherwise = dictGet k kvs

-- Coloca um novo valor no dicionário substituindo o valor já existe
-- pelo novo caso a chave já esteja presente
dictPut :: Eq k => k -> v -> Dict k v -> Dict k v
dictPut k v [] = [(k, v)]
dictPut k v ((k0, v0) : kvs)
  | k == k0 = (k, v) : kvs
  | otherwise = (k0, v0) : dictPut k v kvs


-- Repreesenta o tabuleiro do jogo da velha
data Tabuleiro = Tabuleiro {
  -- Armazena o dicionário com as casas do jogo
  casas :: Dict Coord ConteudoCasa,
  -- Armazena o jogador que tem a vez
  vez :: Jogador,
  -- Lista infinita de números aleatórios
  aleatorios :: [Int]
  }

-- Jogadodes podem usar o X ou o O para marcar as casas escolhidas
data Jogador = X | O deriving (Eq, Show)

-- Armazena cada casa que pode estar Vazia ou Preencida com a marca de
-- um Jogador
data ConteudoCasa = Vazia | Preenchida Jogador deriving Eq

-- Indica a situação atual do jogo.
data Situacao = EmJogo | Empate | Vitoria Jogador deriving Show

-- State sobre o Tabuleiro
type Jogo a = State Tabuleiro a

-- Dada uma lista de números aleatórios cria um tabuleiro com as casas
-- vazias e a vez do primeiro definida aleatóriamente
criaTabuleiro :: [Int] -> Tabuleiro
criaTabuleiro (r:rs) =
  Tabuleiro cs v (abs <$> rs)
  where
    v = if even r then X  else O
    cs = [((x,y), Vazia)| x <- [0..2], y <- [0..2]]

-- Dado o jogador atual, devolve o próximo jogador
proximoJogador :: Jogador -> Jogador
proximoJogador X = O
proximoJogador O = X

-- Dentro da mônada Jogo, devolve um número aleatório obtido da lista
-- infinita fornecida na criação do tabuleiro. Atualiza o estado do
-- tabuleiro para marcar o consumo do número aleatório.
getAleatorio :: Jogo Int
getAleatorio = do
  ~(n:ns) <- gets aleatorios
  modify (\tab -> tab{aleatorios = ns})
  return n

-- Pega uma casa vazia aleatoriamente, caso exista.
getCasaVaziaAleatoria :: Jogo (Maybe Coord)
getCasaVaziaAleatoria = do
  casasVazias <- filter (\c -> Vazia == snd c) <$> gets casas
  if null casasVazias
    then return Nothing
    else do
      n <- fmap (`rem` length casasVazias) getAleatorio
      return . Just $ fst (casasVazias !! n)

-- Passa a vez para o próximo jogador
passaAVez :: Jogo ()
passaAVez = do
  j <- gets vez
  modify (\tab -> tab{vez  = proximoJogador j})


-- Dada uma coordenada de uma casa vazia, faz uma jogada naquela
-- posição para o jogador atualmente com a vez
fazJogada :: Coord -> Jogo ()
fazJogada coord = do
  jog <- gets vez
  modify (\tab -> tab{casas = dictPut coord (Preenchida jog) (casas tab)})
  passaAVez

-- Devolve a situação atual do jogo seja ela empate, em jogo ou
-- vitória de um dos jogadores. Neste último caso, indica o vencedor.
vencedor :: Jogo Situacao
vencedor = do
  dic <- gets casas
  let vencs =
        ganha (linhas dic)    <>
        ganha (colunas dic)   <>
        ganha (diagonais dic)
  return $ case vencs of
    ((Preenchida x:_):_) -> Vitoria x
    _ -> EmJogo
  where
    cs xss dic = fmap (fmap (\c -> fromJust $ dictGet c dic)) xss
    ganha      = filter (\xs -> length (nub xs) == 1)
    linhas     = cs [[(i, j) | j <- [0..2]] | i <- [0..2]]
    colunas    = cs [[(i, j) | i <- [0..2]] | j <- [0..2]]
    diagonais  = cs [[(0,0), (1, 1), (2, 2)], [(0,2), (1, 1), (2, 0)]]

-- Efetua umaa única jogada aleatória, caso possível.
jogaUmaVez :: Jogo Situacao
jogaUmaVez = do
  mc <- getCasaVaziaAleatoria
  case mc of
    Nothing -> return Empate
    Just c  -> do
      fazJogada c
      vencedor

-- Joga o tabuleiro atual até o fim do jogo
jogaAteOFim :: Jogo Situacao
jogaAteOFim = do
  sit <- jogaUmaVez
  case sit of
    EmJogo -> jogaAteOFim
    _      -> return sit

main :: IO ()
main = do
  -- Obtem uma lista infinita de números aleatórios
  gen <- getStdGen
  let rns = randoms gen
  -- Usa essa lista para criar tabuleiro inicial, rodar o jogo e
  -- imprimir o vencedor (ou se houve empate) ao final
  let v = eval jogaAteOFim (criaTabuleiro rns)
  print v

