{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid reverse" #-}
module Idiom (achaListaPalavrasErradas, inicializa) where
import qualified Data.Map as M
import qualified Data.List as L
--import qualified Data.Text as T
import Data.Char
import Control.Monad.State
import qualified Data.Set as Set
import System.Process
import Data.Maybe (fromMaybe)

-- ########## Funções monadicas ##########

type Dict k v = [(k, v)]

data Lingua = English | French | German | Portuguese | Spanish deriving (Enum,Show, Eq, Ord)

newtype Treinamentos = Treinamentos {
    dictTreinamentos  :: Dict Lingua [(String, Int)]
}

data EstadoAplicacao = EstadoAplicacao {
    treinamentos    :: Treinamentos,
    linguaAtual     :: Lingua,
    linguaAspell    :: Dict Lingua String

}

type Estado a = State EstadoAplicacao a

achaLingua :: [(String, Int)] -> Estado  String
achaLingua comparado = do
    -- pega os treinamentos
    ts <- gets (dictTreinamentos . treinamentos)
    -- linguas treinadas
    let ls = keys ts
    -- cria lista de comparacoes
    let comps =  fmap (fmap (`encontraDiferencaAbs` comparado)) (values ts)
    -- seleciona a lingua 
    let l = snd $ minimum $ zip (map sum comps) ls
    -- guarda lingua no estado
    modify $ \s -> s{linguaAtual = l}
    
    -- pega o correspondente do dicionario aspell
    lDict <- gets linguaAspell    
    
    
    let aspell_dict = dictGet l lDict
    
    return $ fromMaybe "" aspell_dict


-- cria lista de frequencia de ngramas a partir dos textos
-- indexa essa lista e coloca em Treinamentos
treinaLinguas :: Dict Lingua String -> Treinamentos
treinaLinguas ls =
    let lstFreqs = map (take 300 . criaListaFrequencia) (values ls) in
        let lstIndxFreqs = map criaListaIndx lstFreqs in
            Treinamentos $ zip (keys ls) lstIndxFreqs

-- atribui e retorna o estado inicial da aplicação, realizando o treinamento
inicializa :: IO EstadoAplicacao
inicializa = do
     -- Lê os arquivos de treinamento
    arquivos <- leArquivos
    
    -- Cria os treinamentos (lista de frequências)
    let trs = treinaLinguas arquivos
    let dictLinguas = zip [English ..] ["en_US", "fr_FR", "de_DE", "pt_BR", "es_LA"]
    
    -- Define o estado inicial com os treinamentos e outros valores padrão
    let estadoInicial = EstadoAplicacao {
            treinamentos = trs,
            linguaAtual = English,  -- Valor padrão
            linguaAspell = dictLinguas
        }
    return estadoInicial
    


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


keys :: Dict k v -> [k]
keys = map fst

values :: Dict k v -> [v]
values = map snd

rodaAspell :: [String] -> String -> IO [String]
rodaAspell palavras lingua = do
    let dict = "--lang=" ++ lingua
    erradas <- readProcess "aspell" [dict, "list"] (unlines palavras)
    return $ removeDuplicados (lines erradas)

-- ########## Funçoes puras ################

removeDuplicados :: (Ord a) => [a] -> [a]
removeDuplicados = Set.toList . Set.fromList


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
    | fst x == fst y  = abs (snd x - snd y)
    | otherwise =  encontraDiferencaAbs x ys


-- poderia colocar tudo junto mas enfim
criaListaNGramasTexto :: String -> [[String]]
criaListaNGramasTexto txt = aplica3Grama $ L.map ajustaString $ words txt

criaListaFrequencia :: String -> [(String, Int)]
criaListaFrequencia txt = ordenaNGramas . dicionario . concat $ criaListaNGramasTexto txt

-- recebe o buffer do texto e o estaod inicial, devolvendo qual lingua o texto esta escrito
rodaEncontraLingua :: String -> EstadoAplicacao -> IO String
rodaEncontraLingua text estadoInicial = do
    -- Cria a lista de frequência de n-gramas do texto de entrada
    let lstFreqComp = take 300 $ criaListaFrequencia text
    let idxCmp = criaListaIndx lstFreqComp
   
    -- Avalia o estado e executa a função achaLingua
    return $ evalState (achaLingua idxCmp) estadoInicial


-- retorna a lista de palavras erradas encontradas pelo aspell
achaListaPalavrasErradas :: String -> EstadoAplicacao -> IO [String]
achaListaPalavrasErradas buffer estadoInicial = do
    l <- rodaEncontraLingua buffer estadoInicial
    rodaAspell (words buffer) l
    
     
