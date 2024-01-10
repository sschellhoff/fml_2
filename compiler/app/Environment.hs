module Environment where

import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad.Trans.Except
import FmlType (FmlType)
import FmlConstant (FmlConstant)

data Environment = Environment
    { envTypes :: Map.Map String FmlType
    , registers :: Map.Map String Int
    , numLabels :: Int
    , constants :: Map.Map FmlConstant Int
    , stackTop :: Int
    }
    deriving (Show)

data EnvironmentError = NotDefinedInEnv String | AlreadyDefinedInEnv String FmlType

emptyEnv :: Environment
emptyEnv = Environment Map.empty Map.empty 0 Map.empty 0

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
    put $ env {envTypes = Map.insert name fmlType (envTypes env), registers = Map.insert name (Map.size (registers env)) (registers env)}

isInEnv :: String -> State Environment Bool
isInEnv name = do
    env <- get
    let isMem = Map.member name (envTypes env)
    return isMem

push :: State Environment Int
push = do
    env <- get
    let newTop = stackTop env + 1
    put $ env {stackTop = newTop}
    return $ newTop + Map.size (registers env)

pop :: Int -> err -> ExceptT err (State Environment) ()
pop addr err = do
    env <- get
    let newTop = stackTop env - 1
    if addr < Map.size (registers env)
        then return () -- do nothing if addr is register
        else if newTop < 0
        then ExceptT $ return $ Left err
        else put $ env {stackTop = newTop}

getLabel :: State Environment Int
getLabel = do
    env <- get
    let newLabel = numLabels env + 1
    put $ env {numLabels = newLabel}
    return newLabel

getLabelName :: State Environment String
getLabelName = do
    l <- getLabel
    return $ "LABEL_" ++ show l

addConstant :: FmlConstant -> State Environment Int
addConstant c = do -- TODO you can do better ;)
    env <- get
    let index = Map.lookup c $ constants env
    case index of
        Just i -> return i
        Nothing -> newIndex where
            newIndex = do
                put $ env {constants = Map.insert c (Map.size (constants env)) (constants env)}
                return $ Map.size (constants env) -- 1

getRegister :: String -> (State Environment) (Maybe Int)
getRegister name = do
    env <- get
    let register = Map.lookup name (registers env)
    return register

getRegisterOrFail :: String -> err -> ExceptT err (State Environment) Int
getRegisterOrFail name err = do
    r <- lift $ getRegister name
    ExceptT $ return $ case r of
        Nothing -> Left err
        Just _r -> Right _r
