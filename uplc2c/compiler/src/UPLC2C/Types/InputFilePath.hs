{-# LANGUAGE NoImplicitPrelude #-}


module UPLC2C.Types.InputFilePath ( InputFilePath (..) ) where


import UPLC2C.Prelude


newtype InputFilePath = InputFilePath { unInputFilePath :: String }
  deriving (Eq, Ord, Show)
