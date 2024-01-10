module Frontend where

import FMLParser
import TypeInference
import Environment
import Control.Monad.Trans.Except
import Control.Monad.State
import qualified FmlError
import qualified TypeCheck
import qualified Ast
import qualified Backend
import CodeGenerator (ByteCode)

-- TODO move backend out of this function
inferAndCheckTypes ::FmlSemanticStep Ast.ParsedProgram ByteCode
inferAndCheckTypes prog = do
    _prog <- TypeInference.addTypeProgramOrFail prog
    _checkedProgram <- TypeCheck.typeCheckProgramOrFail _prog
    Backend.generate _checkedProgram

 -- TODO change type signature
runStringOrFail :: String -> String -> Either      FmlError.FmlError      (Either FmlError.FmlError ByteCode, Environment)
runStringOrFail filename content = do
    ast <- FMLParser.parseContentOrFail filename content
    return $ runState (runExceptT (inferAndCheckTypes ast)) Environment.emptyEnv

 -- TODO change type signature, move to other module
runFileOrFail :: String -> IO      (Either         FmlError.FmlError         (Either FmlError.FmlError ByteCode, Environment))
runFileOrFail filename = do
    runStringOrFail filename <$> readFile filename
