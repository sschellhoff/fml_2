module FmlError where

import Ast

data FmlError = VarNotDef String Ast.ParseInfo | ParseError String
    deriving (Show)
