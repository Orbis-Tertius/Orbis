{-# LANGUAGE NoImplicitPrelude #-}


module UPLC2C.Types.DeBruijnIndex ( DeBruijnIndex (..) ) where


import UPLC2C.Prelude


newtype DeBruijnIndex = DeBruijnIndex { unDeBruijnIndex :: Int }
  deriving (Eq, Ord, Show)
