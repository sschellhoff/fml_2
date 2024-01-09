module Frontend where

import FMLParser
import TypeInference
import Environment
import Control.Monad.Trans.Except
import Control.Monad.State
import qualified FmlError
import qualified TypeCheck
import qualified Ast

inferAndCheck ::FmlSemanticStep Ast.ParsedProgram TypedProgram
inferAndCheck prog = do
    _prog <- TypeInference.addTypeProgramOrFail prog
    TypeCheck.typeCheckProgramOrFail _prog

 -- TODO change type signature
runStringOrFail :: String -> String -> Either      FmlError.FmlError      (Either FmlError.FmlError TypedProgram, Environment)
runStringOrFail filename content = do
    ast <- FMLParser.parseContentOrFail filename content
    return $ runState (runExceptT (inferAndCheck ast)) Environment.emptyEnv

 -- TODO change type signature
runFileOrFail :: String -> IO      (Either         FmlError.FmlError         (Either FmlError.FmlError TypedProgram, Environment))
runFileOrFail filename = do
    runStringOrFail filename <$> readFile filename
