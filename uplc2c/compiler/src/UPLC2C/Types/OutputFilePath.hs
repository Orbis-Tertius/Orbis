{-# LANGUAGE NoImplicitPrelude #-}


module UPLC2C.Types.OutputFilePath ( OutputFilePath (..) ) where


import UPLC2C.Prelude


newtype OutputFilePath = OutputFilePath { unOutputFilePath :: String }
  deriving (Eq, Ord, Show)
