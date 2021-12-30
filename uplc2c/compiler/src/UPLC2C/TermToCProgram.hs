{-# LANGUAGE NoImplicitPrelude #-}


module UPLC2C.TermToCProgram ( termToCProgram ) where


import UPLC2C.Prelude
import UPLC2C.Types.CProgram (CProgram (..))
import UPLC2C.Types.CProgramBuilder (CProgramBuilder (..))
import UPLC2C.Types.UPLCTerm (UPLCTerm)


termToCProgram :: ( Monad m, CProgramBuilder m ) => UPLCTerm -> m CProgram
termToCProgram _ = do
  return () -- TODO
  getProgram
