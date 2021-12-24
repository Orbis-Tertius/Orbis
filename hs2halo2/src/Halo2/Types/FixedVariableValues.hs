{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.FixedVariableValues ( FixedVariableValues (FixedVariableValues, getFixedVariableValues) ) where


import Halo2.Prelude
import Halo2.Types.FieldElement (FieldElement)
import Halo2.Types.PolynomialVariable (PolynomialVariable)


newtype FixedVariableValues = FixedVariableValues
  { getFixedVariableValues :: Map PolynomialVariable FieldElement }
  deriving (Eq, Ord, Generic, Show)
