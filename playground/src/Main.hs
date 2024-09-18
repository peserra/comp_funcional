module Main (main) where
import System.Process

encontraPalavrasErradas :: [String] -> String -> IO [String]
encontraPalavrasErradas palavras lingua = do
    let dict = "--lang=" ++ lingua
    let texto = unlines palavras
    erradas <- readProcess "aspell" [dict, "list"] texto
    return (lines erradas)

main :: IO ()
main = do
    let texto = "so sei que nada to sabendo doido"
    let palavras = words texto
    let lingua = "pt_br"
    output <- encontraPalavrasErradas palavras lingua
    print output
