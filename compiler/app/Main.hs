module Main where
import Compile

main :: IO ()
main = do
    filename <- getLine
    result <- Compile.runFileOrFail filename
    case result of
        Left s -> putStrLn ("Error " ++ show s)
        Right s -> do
             print s
