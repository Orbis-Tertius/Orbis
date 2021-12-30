{-# LANGUAGE NoImplicitPrelude #-}


module UPLC2C.Types.DeBruijnIndex ( DeBruijnIndex (..) ) where


import UPLC2C.Prelude


-- Note that de Bruijn indices are one based!
newtype DeBruijnIndex = DeBruijnIndex { unDeBruijnIndex :: Int }
  deriving (Eq, Ord, Show)
