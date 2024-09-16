module Idiom (detectLanguage) where
import qualified Data.Map as M
import qualified Data.List as L
--import qualified Data.Text as T
import Data.Char
import Control.Monad.State

type Dict k v = [(k, v)]

data Lingua = English | French | German | Portuguese | Spanish deriving (Enum,Show, Eq, Ord)

newtype Treinamentos = Treinamentos {
    dictTreinamentos  :: Dict Lingua [(String, Int)]
} 

data EstadoAplicacao = EstadoAplicacao {
    treinamentos    :: Treinamentos,
    linguaAtual     :: Lingua,
    conteudoTexto   :: [String]
}

type Estado a = State EstadoAplicacao a

pegaConteudo :: [String] -> Estado ()
pegaConteudo bs = modify $ \s -> s{conteudoTexto = bs}


achaLingua :: [(String, Int)] -> Estado Lingua
achaLingua cmps = do
    -- pega os treinamentos
    ts <- gets treinamentos
    -- determina a lingua
    l <- fmap (fmap (`encontraDiferencaAbs` cmps)) (values ts) 
    modify $ \s -> s{linguaAtual = l}
    return l


treinaLinguas :: Dict Lingua String -> Treinamentos
treinaLinguas ls =
    let lstFreqs = map (take 300 . criaListaFrequencia) (values ls) in
    -- associando cada lista de frequencia a uma entrada da lista
        zip (keys ls) (map fst ls (map criaListaIndx lstFreqs))


leArquivos:: IO (Dict Lingua String)
leArquivos = do
    eng <- readFile "TextosInput/eng.txt"
    frn <- readFile "TextosInput/frn.txt"
    ger <- readFile "TextosInput/ger.txt"
    por <- readFile "TextosInput/por.txt"
    spn <- readFile "TextosInput/spn.txt"

     -- cria listas das top frequencias indexadas dos arquivos para referencia
    let listaRefs = [eng, frn, ger, por, spn]
    return $ zip [English ..] listaRefs 


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

keys :: Dict k v -> [k]
keys d = map fst d

values :: Dict k v -> [v]
values d = map snd d




criaListaIndx :: [(String, Int)] -> [(String, Int)]
criaListaIndx xs = zip (map fst xs) [0 ..]

-- transforma string em lowercase e remove coisas que nao sao letras
ajustaString :: [Char] -> [Char]
ajustaString = map toLower . filter isLetter

--retorna um dicionario contendo a frequencia de cada ngrama (logica tirada do GPT, ajustada)
dicionario :: [String] -> [(String, Int)]
dicionario ngramas = M.toList (M.fromListWith (+) $ zip ngramas [1,1 ..])

ordenaNGramas :: [(String, Int)] -> [(String, Int)]
ordenaNGramas  = reverse . L.sortOn snd

-- gera n gramas de uma palavra
geraNGrama :: Int -> String -> [String]
geraNGrama n xs
    | n <= length xs = take n xs : geraNGrama n  (drop 1 xs)
    | otherwise = []

aplica3Grama :: [String] -> [[String]]
aplica3Grama = L.map (geraNGrama 3)

encontraDiferencaAbs :: (Num a1, Eq a2) => (a2, a1) -> [(a2, a1)] -> a1
encontraDiferencaAbs _ [] = 1000 
encontraDiferencaAbs x (y:ys)
    | fst x == fst y  = abs(snd x - snd y)
    | otherwise =  encontraDiferencaAbs x ys


-- poderia colocar tudo junto mas enfim
criaListaNGramasTexto :: String -> [[String]]
criaListaNGramasTexto txt = aplica3Grama $ L.map ajustaString $ words txt

criaListaFrequencia :: String -> [(String, Int)]
criaListaFrequencia txt = ordenaNGramas . dicionario . concat $ criaListaNGramasTexto txt

detectLanguage text = do
       
    -- cria separadamente a lista de frequencia e o indexamento do ngrama do texto de input
    let lstFreqComp =  take 300 $ criaListaFrequencia text
    let idxCmp = criaListaIndx lstFreqComp

    -- lista de comparação do perfil de cada linguagem com o texto a comparar
    --let listComp = fmap (fmap (`encontraDiferencaAbs` idxCmp)) freqIndx
    return $ achaLingua idxCmp