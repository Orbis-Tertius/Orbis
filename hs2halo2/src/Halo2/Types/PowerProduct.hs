{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.PowerProduct ( PowerProduct (PowerProduct, getPowerProduct) ) where


import Halo2.Prelude
import Halo2.Types.Exponent (Exponent)
import Halo2.Types.PolynomialVariable (PolynomialVariable)


newtype PowerProduct = PowerProduct { getPowerProduct :: Map PolynomialVariable Exponent }
  deriving (Eq, Ord, Show, Generic)
