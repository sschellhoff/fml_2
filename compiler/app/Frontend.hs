module Frontend where

import FMLParser
import TypeInference
import Environment
import Control.Monad.Trans.Except
import Control.Monad.State
import qualified FmlError

-- TODO change type signature
runStringOrFail :: String -> String -> Either      FmlError.FmlError      (Either FmlError.FmlError TypedProgram, Environment)
runStringOrFail filename content = do
    ast <- FMLParser.parseContentOrFail filename content
    return $ runState (runExceptT (TypeInference.addTypeProgramOrFail ast)) Environment.emptyEnv

runFileOrFail :: String -> IO      (Either         FmlError.FmlError         (Either FmlError.FmlError TypedProgram, Environment))
runFileOrFail filename = do
    runStringOrFail filename <$> readFile filename
