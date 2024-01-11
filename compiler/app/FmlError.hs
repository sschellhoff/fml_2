module FmlError where

import Ast
import FmlType

data FmlError = VarNotDef String Ast.ParseInfo
            | ParseError String
            | TypeMismatch FmlType.FmlType FmlType.FmlType Ast.ParseInfo
            | InfixOperandMismatch FmlType.FmlType InfixOp FmlType.FmlType Ast.ParseInfo
            | PrefixOperandMismatch PrefixOp FmlType.FmlType Ast.ParseInfo
            | AssignmentTypeMismatch FmlType.FmlType FmlType.FmlType Ast.ParseInfo
            | InvalidConditionType FmlType.FmlType Ast.ParseInfo
            | IllegalStatePop Ast.ParseInfo
    deriving (Show)
