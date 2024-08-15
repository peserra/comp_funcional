module Main (main) where
import qualified Data.Map as M
import qualified Data.List as L 
import Data.Char

-- transforma string em lowercase e remove coisas que nao sao letras
ajustaString :: String -> String
ajustaString [] = []
ajustaString (x:xs)
    | isLetter x =  toLower x : ajustaString xs
    | otherwise = ajustaString xs

--ajustaString2 :: [Char] -> [Char]
--ajustaString2 = map toLower . filter isLetter


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

main :: IO ()
main = do
  conteudo <- readFile "/home/psrr/code/haskell/Projeto1/DetectIdiom/TextosInput/english.txt"
  --let listaFequencia = reverse $ ordenaNGramas $ dicionario $ geraNGrama 3 ( Data.List.map ajustaString $ words conteudo)
  let ngramas = aplica3Grama $ L.map ajustaString $ words conteudo
  let listaFrequencia = ordenaNGramas . dicionario $ concat ngramas

  -- filtrar os top n-gramas que serao utilizados
  -- criar dicionarios das diferentes linguas
  -- criar uma funcao que decide a distancia entre n-gramas de dois exemplos de texto (matching)
  
  print listaFrequencia