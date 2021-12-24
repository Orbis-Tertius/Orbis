{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.PolynomialConstraints
  ( PolynomialConstraints (PolynomialConstraints, getPolynomialConstraints)
  ) where


import Halo2.Prelude
import Halo2.Types.Polynomial (Polynomial)


newtype PolynomialConstraints = PolynomialConstraints
  { getPolynomialConstraints :: [Polynomial] }
  deriving (Eq, Ord, Show, Generic)
