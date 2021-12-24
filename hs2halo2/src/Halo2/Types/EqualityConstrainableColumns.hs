{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.EqualityConstrainableColumns
  ( EqualityConstrainableColumns
    ( EqualityConstrainableColumns
    , getEqualityConstrinableColumns
    )
  ) where


import Halo2.Prelude


newtype EqualityConstrainableColumns = EqualityConstrainableColumns { getEqualityConstrainableColumns :: Set ColumnIndex }
  deriving (Eq, Ord, Show, Generic)
