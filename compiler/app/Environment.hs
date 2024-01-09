module Environment where

import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad.Trans.Except
import FmlType (FmlType)

newtype Environment = Environment
    { envTypes :: Map.Map String FmlType
    }
    deriving (Show)

data EnvironmentError = NotDefinedInEnv String | AlreadyDefinedInEnv String FmlType

emptyEnv :: Environment
emptyEnv = Environment Map.empty

getInEnv :: String -> (State Environment) (Maybe FmlType)
getInEnv name = do
    env <- get
    let fmlType = Map.lookup name (envTypes env)
    return fmlType

getInEnvOrFail :: String -> err -> ExceptT err (State Environment) FmlType
getInEnvOrFail name err = do
    t <- lift $ getInEnv name
    ExceptT $ return $ case t of
        Nothing -> Left err
        Just _t -> Right _t

setInEnv :: String -> FmlType -> State Environment ()
setInEnv name fmlType = do
    env <- get
    put $ env {envTypes = Map.insert name fmlType (envTypes env)}

isInEnv :: String -> State Environment Bool
isInEnv name = do
    env <- get
    let isMem = Map.member name (envTypes env)
    return isMem