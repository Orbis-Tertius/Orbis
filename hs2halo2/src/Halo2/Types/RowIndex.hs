{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.RowIndex ( RowIndex (RowIndex, getRowIndex ) ) where


import Halo2.Prelude


newtype RowIndex = RowIndex { getRowIndex :: Int }
  deriving (Num, Enum, Real, Integral, Eq, Ord, Show, Generic)
