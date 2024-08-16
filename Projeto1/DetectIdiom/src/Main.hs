module Main (main) where
import qualified Data.Map as M
import qualified Data.List as L
--import qualified Data.Text as T
import Data.Char

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

montaArvBusca :: [a] -> Tree a
montaArvBusca [a] = Leaf a
montaArvBusca xs = Node (montaArvBusca ls) (head rs) (montaArvBusca (drop 1 rs))
  where
    (ls, rs) = separaLista xs

separaLista :: [a] -> ([a],[a])
separaLista xs = splitAt (length xs `div` 2) xs

extraiNGramas :: [(a, b)] -> [a]
extraiNGramas xs = [fst w | w <- xs]

criaListaIndx :: [(String, Int)] -> [(String, Int)]
criaListaIndx xs = zip (extraiNGramas xs) [0 ..]

buscaNaArvore x (Leaf v)
    | fst x /= fst v = 300
    | otherwise = abs(snd x - snd v)
buscaNaArvore x  (Node l v r)
    | fst x == fst v = abs(snd x - snd v)
    | snd x < snd v = buscaNaArvore x l
    | otherwise = buscaNaArvore x r


-- transforma string em lowercase e remove coisas que nao sao letras
ajustaString :: [Char] -> [Char]
ajustaString = map toLower . filter isLetter


-- gera n gramas de uma palavra
geraNGrama :: Int -> String -> [String]
geraNGrama n xs
    | n <= length xs = take n xs : geraNGrama n  (drop 1 xs)
    | otherwise = []

--retorna um dicionario contendo a frequencia de cada ngrama
dicionario :: [String] -> [(String, Int)]
dicionario ngramas = M.toList (M.fromListWith (+) [(ngrama, 1)| ngrama <- ngramas])

ordenaNGramas :: [(String, Int)] -> [(String, Int)]
ordenaNGramas  = reverse . L.sortOn snd

aplica3Grama :: [String] -> [[String]]
aplica3Grama = L.map (geraNGrama 3)


-- poderia colocar tudo junto mas enfim
criaListaNGramasTexto :: String -> [[String]]
criaListaNGramasTexto txt = aplica3Grama $ L.map ajustaString $ words txt

criaListaFrequencia :: String -> [(String, Int)]
criaListaFrequencia txt = ordenaNGramas . dicionario $ concat $ criaListaNGramasTexto txt


encontraDiferencaAbs :: (Num a1, Eq a2) => (a2, a1) -> [(a2, a1)] -> a1
encontraDiferencaAbs _ [] = 500 
encontraDiferencaAbs x (y:ys)
    | fst x == fst y  = abs(snd x - snd y)
    | otherwise =  encontraDiferencaAbs x ys


main :: IO ()
main = do
    refEng <- readFile "/home/pserra/code/haskell/Projeto1/DetectIdiom/TextosInput/eng.txt"
    refFrn <- readFile "/home/pserra/code/haskell/Projeto1/DetectIdiom/TextosInput/frn.txt"
    refGer <- readFile "/home/pserra/code/haskell/Projeto1/DetectIdiom/TextosInput/ger.txt"
    refPor <- readFile "/home/pserra/code/haskell/Projeto1/DetectIdiom/TextosInput/por.txt"
    refSpn <- readFile "/home/pserra/code/haskell/Projeto1/DetectIdiom/TextosInput/spn.txt"
    teste <- readFile "/home/pserra/code/haskell/Projeto1/DetectIdiom/TextosInput/teste.txt"


    let listaFrequenciaEng =  take 300 $ criaListaFrequencia refEng
    let listaFrequenciaFrn =  take 300 $ criaListaFrequencia refFrn
    let listaFrequenciaGer =  take 300 $ criaListaFrequencia refGer
    let listaFrequenciaPor =  take 300 $ criaListaFrequencia refPor
    let listaFrequenciaSpn =  take 300 $ criaListaFrequencia refSpn
    let listaFrequenciaTst =  take 300 $ criaListaFrequencia teste
  
  --let arv = montaArvBusca $ map criaListaIndx fst listaFrequenciaEng

  -- filtrar os top n-gramas que serao utilizados
  -- criar dicionarios das diferentes linguas
  -- criar uma funcao que decide a distancia entre n-gramas de dois exemplos de texto (matching)
    let idxEng = criaListaIndx listaFrequenciaEng
    let idxFrn = criaListaIndx listaFrequenciaFrn
    let idxGer = criaListaIndx listaFrequenciaGer
    let idxPor = criaListaIndx listaFrequenciaPor
    let idxSpn = criaListaIndx listaFrequenciaSpn
    let idxTst = criaListaIndx listaFrequenciaTst

    let compEng = sum [encontraDiferencaAbs i idxTst | i <- idxEng]
    let compFrn = sum [encontraDiferencaAbs i idxTst | i <- idxFrn]
    let compGer = sum [encontraDiferencaAbs i idxTst | i <- idxGer]
    let compPor = sum [encontraDiferencaAbs i idxTst | i <- idxPor]
    let compSpn = sum [encontraDiferencaAbs i idxTst | i <- idxSpn]

    let listLang = [(compEng, "Ingles"), ( compFrn, "Frances"), ( compGer, "Alemao"),(compPor, "Portugues"),(compSpn,"Espanhol")]
    print ( snd $ minimum listLang)
    --print (montaArvBusca $ criaListaIndx listaFrequenciaEng)
    --putStrLn refPor