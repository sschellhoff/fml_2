module Frontend where

import Ast
import FMLParser
import TypeInference
import Text.Megaparsec (ParseErrorBundle)
import Data.Text (Text)
import Data.Void (Void)

runString :: String -> String -> Either (ParseErrorBundle Text Void) TypeInference.TypedProgram
runString filename content = typedAst where
    ast = FMLParser.parseContent filename content
    typedAst = fmap TypeInference.addTypeProgram ast

runFile :: String -> IO (Either (ParseErrorBundle Text Void) TypedProgram)
runFile filename = do
    runString filename <$> readFile filename
