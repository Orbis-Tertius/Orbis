{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.RowCount ( RowCount (RowCount, getRowCount) ) where


import Halo2.Prelude


newtype RowCount = RowCount { getRowCount :: Int }
  deriving (Eq, Ord, Show, Generic)
