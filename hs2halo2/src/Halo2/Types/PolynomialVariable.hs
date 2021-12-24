{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.PolynomialVariable ( PolynomialVariable (PolynomialVariable) ) where


import Halo2.Prelude
import Halo2.Types.ColumnIndex
import Halo2.Types.RowIndex


data PolynomialVariable =
  PolynomialVariable
  { colIndex :: ColumnIndex
  , rowIndex :: RowIndex }
  deriving (Eq, Ord, Show, Generic)
