module Idiom2 (detectIdiom) where
import qualified Data.Map as M
import qualified Data.List as L
--import qualified Data.Text as T
import Data.Char
import Control.Monad.State

-- Definindo Languages, para ter o nome das linguagens
data Languages = English | French | German | Portuguese | Spanish 
                    deriving (Enum, Show, Eq, Ord)


-- usado para guardar o state das listas de comparação
data IdiomState = IdiomState{
    english     :: [(String, Int)],
    french      :: [(String, Int)],
    german      :: [(String, Int)],
    portuguese  :: [(String, Int)],
    spanish     :: [(String, Int)] 
} deriving Show


carregaTreinamento :: IO IdiomState
carregaTreinamento = do 
    let tamLista = 300 
    
    eng <- readFile "TextosInput/eng.txt"
    frn <- readFile "TextosInput/frn.txt"
    ger <- readFile "TextosInput/ger.txt"
    por <- readFile "TextosInput/por.txt"
    spn <- readFile "TextosInput/spn.txt"

     -- cria listas das top frequencias indexadas dos arquivos para referencia
    let listaRefs = [eng, frn, ger, por, spn]
    let lstFreqs = map (take tamLista . criaListaFrequencia) listaRefs
    -- associando cada lista de frequencia a uma entrada da lista
    let [engFreq, frnFreq, gerFreq, porFreq, spnFreq] = map criaListaIndx lstFreqs
    
    -- remontando state, com os valores das listas preenchidos
    return IdiomState {
        english     = engFreq,
        french      = frnFreq,
        german      = gerFreq,
        portuguese  = porFreq,
        spanish     = spnFreq
    }


getEnglishFreq :: State IdiomState [(String, Int)]
getEnglishFreq = gets english

getFrenchFreq :: State IdiomState [(String, Int)]
getFrenchFreq = gets french

getGermanFreq :: State IdiomState [(String, Int)]
getGermanFreq = gets german

getPortugueseFreq :: State IdiomState [(String, Int)]
getPortugueseFreq = gets portuguese

getSpanishFreq :: State IdiomState [(String, Int)]
getSpanishFreq = gets spanish


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

detectIdiom :: String -> State IdiomState Languages
detectIdiom text = do
    let tamLista = 300 
    -- cria separadamente a lista de frequencia e o indexamento do ngrama do texto de input
    let lstFreqComp =  take tamLista $ criaListaFrequencia text
    let idxCmp = criaListaIndx lstFreqComp

    -- carrega treinamentos
    engFreq <- getEnglishFreq
    frnFreq <- getFrenchFreq
    gerFreq <- getGermanFreq
    porFreq <- getPortugueseFreq
    spnFreq <- getSpanishFreq
    
    let freqIndx = [engFreq, frnFreq, gerFreq, porFreq, spnFreq]

    -- lista de comparação do perfil de cada linguagem com o texto a comparar
    let listComp = fmap (fmap (`encontraDiferencaAbs` idxCmp)) freqIndx

    let listLang = zip (map sum listComp) [English ..]
    return $ snd $ minimum listLang