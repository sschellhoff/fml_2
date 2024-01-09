module FmlType where

data PrimitiveType = FmlBoolean | FmlInt64 | FmlFloat64
    deriving (Show)

data FmlType = Primitive PrimitiveType | Unit
    deriving (Show)