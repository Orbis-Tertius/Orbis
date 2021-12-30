{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module UPLC2C.TermToCProgram ( termToCProgram ) where


import Data.Text (pack)
import PlutusCore.DeBruijn (DeBruijn (..), Index (..))
import UntypedPlutusCore.Core.Type (Term (Var))

import UPLC2C.Prelude
import UPLC2C.Types.CFunctionDefinition (CFunctionDefinition (..))
import UPLC2C.Types.CName (CName (..))
import UPLC2C.Types.CProgramBuilder (CProgramBuilder (..))
import UPLC2C.Types.DeBruijnIndex (DeBruijnIndex (..))
import UPLC2C.Types.UPLCTerm (UPLCTerm)


termToCProgram :: ( Monad m, CProgramBuilder m ) => UPLCTerm -> m ()
termToCProgram =
  \case
    Var _ ix@(DeBruijn (Index i)) ->
      addToProgram (deBruijnIndexToCName ix) (VariableReference (DeBruijnIndex (fromIntegral i)))


deBruijnIndexToCName :: DeBruijn -> CName
deBruijnIndexToCName (DeBruijn i) = CName $ "lookup_var_" <> pack (show i)
