{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module UPLC2C.TermToCProgram ( termToCProgram ) where


import Data.Text (pack)
import PlutusCore.DeBruijn (DeBruijn (..), Index (..))
import qualified UntypedPlutusCore.Core.Type as UPLC

import UPLC2C.Prelude
import UPLC2C.Types.CFunctionDefinition (CFunctionDefinition (..))
import UPLC2C.Types.CName (CName (..))
import UPLC2C.Types.CProgramBuilder (CProgramBuilder (..))
import UPLC2C.Types.DeBruijnIndex (DeBruijnIndex (..))
import UPLC2C.Types.UPLCTerm (UPLCTerm)


termToCProgram :: ( Monad m, CProgramBuilder m ) => UPLCTerm -> m CName
termToCProgram =
  \case
    UPLC.Var _ ix@(DeBruijn (Index i)) -> do
      let name = deBruijnIndexToCName ix
      addToProgram name (VariableReference (DeBruijnIndex (fromIntegral i)))
      return name
    UPLC.LamAbs _ _ subterm -> do
      subtermName <- termToCProgram subterm
      absName <- genSym
      addToProgram absName (CreateClosureOver subtermName)
      return absName
    UPLC.Apply _ operator operand -> do
      operatorName <- termToCProgram operator
      operandName <- termToCProgram operand
      applyName <- genSym
      addToProgram applyName (Apply operatorName operandName)
      return applyName
    UPLC.Force _ operand -> do
      operandName <- termToCProgram operand
      forceName <- genSym
      addToProgram forceName (Force operandName)
      return forceName
    UPLC.Delay _ operand -> do
      operandName <- termToCProgram operand
      delayName <- genSym
      addToProgram delayName (Delay operandName)
      return delayName
    UPLC.Constant _ _ -> todo
    UPLC.Builtin _ _ -> todo
    UPLC.Error _ -> return (CName "builtin_error")


deBruijnIndexToCName :: DeBruijn -> CName
deBruijnIndexToCName (DeBruijn i) = CName $ "lookup_var_" <> pack (show i)


todo :: a
todo = todo
