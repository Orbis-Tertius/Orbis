{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.CircuitConfig ( CircuitConfig (CircuitConfig) ) where


import Halo2.Prelude
import Halo2.Types.ColumnTypes (ColumnTypes)
import Halo2.Types.EqualityConstrainableColumns (EqualityConstrainableColumns)
import Halo2.Types.FiniteField (FiniteField)
import Halo2.Types.LookupArguments (LookupArguments)
import Halo2.Types.PolynomialConstraints (PolynomialConstraints)
import Halo2.Types.PolynomialDegreeBound (PolynomialDegreeBound)


data CircuitConfig =
  CircuitConfig
  { field :: FiniteField
  , columnTypes :: ColumnTypes
  , equalityConstrainableColumns :: EqualityConstrainableColumns
  , polynomialDegreeBound :: PolynomialDegreeBound
  , polynomialConstraints :: PolynomialConstraints
  , lookupArguments :: LookupArguments
  }
  deriving (Eq, Ord, Show, Generic)
