{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.Polynomial ( Polynomial (Polynomial) ) where


import Halo2.Prelude
import Halo2.Types.Monomial (Monomial)


newtype Polynomial = Polynomial { monos :: [Monomial] }
  deriving (Eq, Ord, Show, Generic)
