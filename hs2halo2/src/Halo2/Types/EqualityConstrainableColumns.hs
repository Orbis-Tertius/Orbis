{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.EqualityConstrainableColumns
  ( EqualityConstrainableColumns
    ( EqualityConstrainableColumns
    , getEqualityConstrainableColumns
    )
  ) where


import Halo2.Prelude
import Halo2.Types.ColumnIndex (ColumnIndex)


newtype EqualityConstrainableColumns = EqualityConstrainableColumns { getEqualityConstrainableColumns :: Set ColumnIndex }
  deriving (Eq, Ord, Show, Generic)
