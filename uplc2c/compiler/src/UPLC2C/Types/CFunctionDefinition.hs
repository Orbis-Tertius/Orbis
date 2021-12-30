{-# LANGUAGE NoImplicitPrelude #-}


module UPLC2C.Types.CFunctionDefinition ( CFunctionDefinition (..) ) where


import UPLC2C.Prelude
import UPLC2C.Types.CName (CName)
import UPLC2C.Types.DeBruijnIndex (DeBruijnIndex)


data CFunctionDefinition =
    VariableReference DeBruijnIndex
  | CreateClosureOver CName
  | Apply CName CName
  | Delay CName
  | Force CName
  -- TODO constants
  deriving (Eq, Ord, Show)
