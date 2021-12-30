{-# LANGUAGE NoImplicitPrelude #-}


module UPLC2C.Types.UPLCTerm ( UPLCTerm ) where


import PlutusCore.DeBruijn (DeBruijn)
import PlutusCore.Default (DefaultUni, DefaultFun)
import UntypedPlutusCore.Core.Type (Term)


type UPLCTerm = Term DeBruijn DefaultUni DefaultFun ()
