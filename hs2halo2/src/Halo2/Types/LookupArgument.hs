{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.LookupArgument ( LookupArgument (LookupArgument) ) where


import Halo2.Prelude

import Halo2.Types.InputExpression (InputExpression)
import Halo2.Types.TableColumn (TableColumn)


newtype LookupArgument = LookupArgument { tableMap :: [(InputExpression, TableColumn)] }
  deriving (Eq, Ord, Show, Generic)
