module Main where
import Frontend

main :: IO ()
main = do
    filename <- getLine
    result <- Frontend.runFile filename
    case result of
        Left s -> putStrLn ("Error " ++ show s)
        Right s -> do
             print s
