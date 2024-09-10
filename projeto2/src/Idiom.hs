module Idiom (detectIdiom) where
import qualified Data.Map as M
import qualified Data.List as L
--import qualified Data.Text as T
import Data.Char


data Languages = English | French | German | Portuguese | Spanish deriving (Enum,Show, Eq, Ord)


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

detectIdiom text = do
    let tamLista = 300 

    eng <- readFile "TextosInput/eng.txt"
    frn <- readFile "TextosInput/frn.txt"
    ger <- readFile "TextosInput/ger.txt"
    por <- readFile "TextosInput/por.txt"
    spn <- readFile "TextosInput/spn.txt"
    cmp <- readFile "TextosInput/app_saved_text.txt"

    -- cria listas das top frequencias indexadas dos arquivos para referencia
    let listaRefs = [eng, frn, ger, por, spn]
    let lstFreqs = map (take tamLista . criaListaFrequencia) listaRefs
    let freqIndx = map criaListaIndx lstFreqs
    
    -- cria separadamente a lista de frequencia e o indexamento do ngrama do texto de input
    let lstFreqComp =  take tamLista $ criaListaFrequencia cmp
    let idxCmp = criaListaIndx lstFreqComp

    -- lista de comparação do perfil de cada linguagem com o texto a comparar
    let listComp = fmap (fmap (`encontraDiferencaAbs` idxCmp)) freqIndx

    let listLang = zip (map sum listComp) [English ..]
    snd $ minimum listLang