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

-- TODO return early on first error to result in a singl either
compile :: String -> String -> Either FmlError.FmlError (Either FmlError.FmlError ByteCode)
compile filename content = do
    ast <- FMLParser.parseContentOrFail filename content
    return $ evalState (runExceptT (transformAst ast)) Environment.emptyEnv

transformAst ::FmlSemanticStep Ast.ParsedProgram ByteCode
transformAst ast = do
    _ast <- TypeInference.addTypeProgramOrFail ast
    _checkedProgram <- TypeCheck.typeCheckProgramOrFail _ast
    Backend.generate _checkedProgram
