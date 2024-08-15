module Main (main) where
import qualified Data.Map as M
import qualified Data.List as L
-- import qualified Data.Text as T
import Data.Char

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

montaArvBusca :: [a] -> Tree a
montaArvBusca [a] = Leaf a
montaArvBusca xs = Node (montaArvBusca ls) (head rs) (montaArvBusca rs)
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

main :: IO ()
main = do
    refEng <- readFile "/home/pserra/code/haskell/Projeto1/DetectIdiom/TextosInput/eng.txt"
    refFrn <- readFile "/home/pserra/code/haskell/Projeto1/DetectIdiom/TextosInput/frn.txt"
    refGer <- readFile "/home/pserra/code/haskell/Projeto1/DetectIdiom/TextosInput/ger.txt"
    refPor <- readFile "/home/pserra/code/haskell/Projeto1/DetectIdiom/TextosInput/por.txt"
    refSpn <- readFile "/home/pserra/code/haskell/Projeto1/DetectIdiom/TextosInput/spn.txt"


    let listaFrequenciaEng =  take 10 $ criaListaFrequencia refEng
  
  --let arv = montaArvBusca $ map criaListaIndx fst listaFrequenciaEng

  -- filtrar os top n-gramas que serao utilizados
  -- criar dicionarios das diferentes linguas
  -- criar uma funcao que decide a distancia entre n-gramas de dois exemplos de texto (matching)
  
    print (montaArvBusca $ criaListaIndx listaFrequenciaEng)