{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.Monomial ( Monomial (Monomial) ) where


import Halo2.Prelude
import Halo2.Types.Coefficient (Coefficient)
import Halo2.Types.PowerProduct (PowerProduct)


data Monomial =
  Monomial
  { coef :: Coefficient
  , vars :: PowerProduct
  }
  deriving (Eq, Ord, Show, Generic)
