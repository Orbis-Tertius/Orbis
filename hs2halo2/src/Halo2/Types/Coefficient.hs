{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.Coefficient ( Coefficient (Coefficient, getCoefficient) ) where


import Halo2.Prelude
import Halo2.Types.FieldElement (FieldElement)


newtype Coefficient = Coefficient { getCoefficient :: FieldElement }
  deriving (Enum, Eq, Ord, Show, Generic)
