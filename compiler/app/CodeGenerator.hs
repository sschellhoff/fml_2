module CodeGenerator where

data OpCode = LABEL Int
            | OP_RETURN
            | OP_ADD Int Int Int
            | OP_SUB Int Int Int
            | OP_MULT Int Int Int
            | OP_DIV Int Int Int
            | OP_MOD Int Int Int
            | OP_AND Int Int Int
            | OP_OR Int Int Int
            | OP_EQ Int Int Int
            | OP_NEQ Int Int Int
            | OP_LT Int Int Int
            | OP_GT Int Int Int
            | OP_NEG Int Int
            | OP_JUMP String
            | OP_JUMPF String Int
            | OP_MOVE Int Int

newtype ByteCode = BypeCode {
    code :: [ByteCode]
}

--generateCode :: SemanticStep TypedProgram ByteCode
--generateCode ast = do
