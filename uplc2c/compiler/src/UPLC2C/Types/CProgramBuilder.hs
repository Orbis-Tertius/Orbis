{-# LANGUAGE NoImplicitPrelude #-}


module UPLC2C.Types.CProgramBuilder ( CProgramBuilder (..) ) where


import UPLC2C.Types.CFunctionDefinition (CFunctionDefinition)
import UPLC2C.Types.CName (CName)
import UPLC2C.Types.CProgram (CProgram)


class CProgramBuilder m where
  getProgram :: m CProgram
  addToProgram :: CName -> CFunctionDefinition -> m ()
  genSym :: m CName
