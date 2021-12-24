{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.FiniteField ( FiniteField (FiniteField) ) where


import Halo2.Prelude


newtype FiniteField = FiniteField { degree :: Int }
  deriving (Num, Enum, Real, Integral, Eq, Ord, Show, Generic)
