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

runFileOrFail :: String -> IO (Either FmlError.FmlError ByteCode)
runFileOrFail filename = do
    compile filename <$> readFile filename

compile :: String -> String -> Either FmlError.FmlError ByteCode
compile filename content = do
    joinEither _compile where
        _compile = do
            ast <- FMLParser.parseContentOrFail filename content
            return $ evalState (runExceptT (transformAst ast)) Environment.emptyEnv

transformAst ::FmlSemanticStep Ast.ParsedProgram ByteCode
transformAst ast = do
    _ast <- TypeInference.addTypeProgramOrFail ast
    _checkedProgram <- TypeCheck.typeCheckProgramOrFail _ast
    Backend.generate _checkedProgram

joinEither :: Either a (Either a b) -> Either a b
joinEither (Left err) = Left err
joinEither (Right (Left err)) = Left err
joinEither (Right (Right value)) = Right value
