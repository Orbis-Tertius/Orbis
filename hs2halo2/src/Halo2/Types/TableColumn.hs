{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.TableColumn ( TableColumn (TableColumn) ) where


import Halo2.Prelude

import Halo2.Types.Polynomial (Polynomial)


newtype TableColumn = TableColumn { expression :: Polynomial }
  deriving (Eq, Ord, Show, Generic)
