module Main where
import Ast
import FMLParser

main :: IO ()
main = do
    filename <- getLine
    FMLParser.parseFile filename
