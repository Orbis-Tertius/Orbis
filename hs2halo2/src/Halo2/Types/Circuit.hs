{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.Circuit ( Circuit (Circuit) ) where


import Halo2.Prelude
import Halo2.Types.CircuitConfig (CircuitConfig)
import Halo2.Types.EqualityConstraints (EqualityConstraints)
import Halo2.Types.FixedVariableValues (FixedVariableValues)
import Halo2.Types.RowCount (RowCount)


data Circuit =
  Circuit
  { config :: CircuitConfig
  , rowCount :: RowCount
  , equalityConstraints :: EqualityConstraints
  , fixedVariableValues :: FixedVariableValues
  }
  deriving (Eq, Ord, Show, Generic)
