module Environment where

import qualified Data.Map.Strict as Map
import Control.Monad.State
import TypeInference (FmlType)

newtype Environment = Environment
    { envTypes :: Map.Map String FmlType
    }

data EnvironmentError = NotDefinedInEnv String | AlreadyDefinedInEnv String FmlType

emptyEnv :: Environment
emptyEnv = Environment Map.empty

getInEnv :: String -> (State Environment) (Maybe FmlType)
getInEnv name = do
    env <- get
    let fmlType = Map.lookup name (envTypes env)
    return fmlType

setInEnv :: String -> FmlType -> State Environment ()
setInEnv name fmlType = do
    env <- get
    let types = envTypes env
    put $ env {envTypes = Map.insert name fmlType (envTypes env)}

isInEnv :: String -> State Environment Bool
isInEnv name = do
    env <- get
    let isMem = Map.member name (envTypes env)
    return isMem