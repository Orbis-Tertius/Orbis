{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.EqualityConstraint ( EqualityConstraint (EqualityConstraint, getEqualityConstraint) ) where


import Halo2.Prelude
import Halo2.Types.PolynomialVariable


newtype EqualityConstraint = EqualityConstraint
  { getEqualityConstraint :: Set PolynomialVariable }
  deriving (Eq, Ord, Generic, Show)
