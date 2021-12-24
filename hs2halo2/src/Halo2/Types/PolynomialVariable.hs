{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.PolynomialVariable ( PolynomialVariable (PolynomialVariable) ) where


import Halo2.Prelude


newtype PolynomialVariable = PolynomialVariable { index :: Int }
  deriving (Num, Enum, Real, Integral, Eq, Ord, Show, Generic)
