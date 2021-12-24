{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.ColumnType
  ( ColumnType (Fixed, Advice, Instance)
  ) where


import Halo2.Prelude


data ColumnType = Fixed | Advice | Instance
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)
