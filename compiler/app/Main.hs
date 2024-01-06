module Main where
import Frontend

main :: IO ()
main = do
    filename <- getLine
    result <- Frontend.runFile filename
    case result of
        Left s -> putStrLn ("Left " ++ show s)
        Right s -> do
             putStrLn ("Right (IO " ++ show s ++ ")")
