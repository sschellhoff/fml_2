module Compile where
import FMLParser
import CodeGenerator (ByteCode)
import qualified Ast
import qualified TypeCheck
import qualified Backend
import qualified FmlError
import Control.Monad.State (evalState)
import Control.Monad.Except (runExceptT)
import qualified Environment
import TypeInference

runFileOrFail :: String -> IO (Either FmlError.FmlError (Either FmlError.FmlError ByteCode))
runFileOrFail filename = do
    compile filename <$> readFile filename

compile :: String -> String -> Either FmlError.FmlError (Either FmlError.FmlError ByteCode)
compile filename content = do
    ast <- FMLParser.parseContentOrFail filename content
    let s = evalState (runExceptT (transformAst ast)) Environment.emptyEnv
    return s

transformAst ::FmlSemanticStep Ast.ParsedProgram ByteCode
transformAst ast = do
    _ast <- TypeInference.addTypeProgramOrFail ast
    _checkedProgram <- TypeCheck.typeCheckProgramOrFail _ast
    Backend.generate _checkedProgram
