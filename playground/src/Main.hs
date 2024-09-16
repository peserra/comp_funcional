module Main (main) where
import System.Process

main :: IO ()
main = do
    output <- readProcess "aspell" ["-s","banna"] ""
    putStrLn output
