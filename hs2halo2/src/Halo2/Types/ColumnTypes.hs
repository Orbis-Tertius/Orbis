{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.ColumnTypes
  ( ColumnTypes (ColumnTypes, getColumnTypes)
  ) where


import Halo2.Prelude
import Halo2.Types.ColumnType (ColumnType)


newtype ColumnTypes = ColumnTypes { getColumnTypes :: [ColumnType] }
  deriving (Eq, Ord, Show, Generic)
