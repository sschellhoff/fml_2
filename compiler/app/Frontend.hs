module Frontend where
import TypeInference
import qualified TypeCheck
import qualified Ast
import qualified TypeInference as TypeInferince

inferAndCheckTypes :: FmlSemanticStep Ast.ParsedProgram TypeInferince.TypedProgram
inferAndCheckTypes prog = do
    _prog <- TypeInference.addTypeProgramOrFail prog
    TypeCheck.typeCheckProgramOrFail _prog
