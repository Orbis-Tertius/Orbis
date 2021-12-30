{-# LANGUAGE NoImplicitPrelude #-}


module UPLC2C.Types.CName ( CName (..) ) where


import UPLC2C.Prelude


-- Represents a C object code identifier.
newtype CName = CName { unCName :: Text }
  deriving (Eq, Ord, Show)
