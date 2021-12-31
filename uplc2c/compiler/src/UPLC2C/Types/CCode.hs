{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module UPLC2C.Types.CCode ( CCode (..) ) where


import UPLC2C.Prelude


newtype CCode = CCode { unCCode :: Text }
  deriving (Eq, Ord, Show, Semigroup, Monoid)
