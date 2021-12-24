{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.PolynomialDegreeBound
  ( PolynomialDegreeBound (PolynomialDegreeBound, getPolynomialDegreeBound)
  ) where


import Halo2.Prelude


newtype PolynomialDegreeBound = PolynomialDegreeBound { getPolynomialDegreeBound :: Int }
  deriving (Eq, Ord, Num, Enum, Real, Integral, Generic, Show)
