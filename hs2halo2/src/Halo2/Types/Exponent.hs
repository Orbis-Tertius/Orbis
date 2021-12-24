{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.Exponent ( Exponent (Exponent, getExponent) ) where


import Halo2.Prelude


newtype Exponent = Exponent { getExponent :: Int }
  deriving (Num, Enum, Real, Integral, Eq, Ord, Show, Generic)
