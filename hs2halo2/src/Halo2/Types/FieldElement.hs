{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.FieldElement ( FieldElement (FieldElement, getFieldElement ) ) where


import Halo2.Prelude


newtype FieldElement = FieldElement { getFieldElement :: Integer }
  deriving (Enum, Eq, Ord, Show, Generic)
