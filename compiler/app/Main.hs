module Main where
import System.Environment
import Compile

getInputFilename :: [String] -> IO String
getInputFilename [] = do
    getLine
getInputFilename (filename:_) = return filename


main :: IO ()
main = do
    args <- getArgs
    --filename <- getLine
    filename <- getInputFilename args
    result <- Compile.runFileOrFail filename
    case result of
        Left s -> putStrLn ("Error " ++ show s)
        Right s -> do
             print s
