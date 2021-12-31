{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module UPLC2C.Compile ( compile ) where


import qualified Data.Map as Map
import Data.Text (pack)
import Text.Printf (printf)

import UPLC2C.CompileFunctionDefinition (compileFunctionDefinition)
import UPLC2C.Prelude
import UPLC2C.Types.CCode (CCode (..))
import UPLC2C.Types.CName (CName (..))
import UPLC2C.Types.CProgram (CProgram (..))
import UPLC2C.Types.CProgramBuilder (CProgramBuilder (getProgram))
import qualified UPLC2C.Types.CProgramBuilderT as CProgramBuilderT
import UPLC2C.Types.UPLCTerm (UPLCTerm)
import UPLC2C.TermToCProgram (termToCProgram)


compile :: MonadIO m => UPLCTerm -> m CCode
compile term = do
  (program, entryPointName) <- CProgramBuilderT.run $ do
    entryPointName <- termToCProgram term
    program <- getProgram
    return (program, entryPointName)
  let code = CCode "#include \"rts.h\"\n\n" <> programCode program <> CCode (pack (printf "\n\n\n\nvoid main() { %s(0); }\n" (unCName entryPointName)))
  return code


programCode :: CProgram -> CCode
programCode program = programFunctionSignatures program <> CCode "\n\n\n\n" <> programFunctionDefinitions program


programFunctionSignatures :: CProgram -> CCode
programFunctionSignatures (CProgram program) =
  mconcat $ functionSignature <$> Map.keys program


functionSignature :: CName -> CCode
functionSignature (CName name) =
  CCode . pack $ printf "struct NFData *%s(struct LexicalScope *scope);\n" name


programFunctionDefinitions :: CProgram -> CCode
programFunctionDefinitions (CProgram program) =
  mconcat . Map.elems $ Map.mapWithKey compileFunctionDefinition program
