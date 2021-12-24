{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Halo2.Types.ColumnIndex
  ( ColumnIndex (ColumnIndex, getColumnIndex)
  ) where


import Halo2.Prelude


newtype ColumnIndex = ColumnIndex { getColumnIndex :: Int }
  deriving (Eq, Ord, Num, Enum, Real, Integral, Show, Generic)
