module FmlConstant where

data FmlConstant = ConstInt64 Integer | ConstFloat64 Double | ConstBool Bool
    deriving (Eq, Ord)

instance Show FmlConstant where
    show (ConstBool b) = "\tb " ++ show b
    show (ConstFloat64 f) = "\tf64 " ++ show f
    show (ConstInt64 i) = "\ti64 " ++ show  i