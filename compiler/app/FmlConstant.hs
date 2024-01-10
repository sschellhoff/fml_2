module FmlConstant where

data FmlConstant = ConstInt64 Integer | ConstFloat64 Double | ConstBool Bool
    deriving (Show, Eq, Ord)
