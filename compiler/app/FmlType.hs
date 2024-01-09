module FmlType where

data PrimitiveType = FmlBoolean | FmlInt64 | FmlFloat64
    deriving (Show, Eq)

data FmlType = Primitive PrimitiveType | Unit
    deriving (Show, Eq)