module Backend where
import TypeInference (FmlSemanticStep, TypedProgram)
import CodeGenerator (ByteCode, generateCodeProgram)

generate :: FmlSemanticStep TypedProgram ByteCode
generate program = do generateCodeProgram program
