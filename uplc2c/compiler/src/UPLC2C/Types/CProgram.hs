{-# LANGUAGE NoImplicitPrelude #-}


module UPLC2C.Types.CProgram ( CProgram (..) ) where


import UPLC2C.Prelude
import UPLC2C.Types.CName (CName)
import UPLC2C.Types.CFunctionDefinition (CFunctionDefinition)


newtype CProgram = CProgram { unCProgram :: Map CName CFunctionDefinition }
  deriving (Eq, Ord, Show)
