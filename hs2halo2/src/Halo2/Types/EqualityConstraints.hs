{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.EqualityConstraints
  ( EqualityConstraints (EqualityConstraints, getEqualityConstraints)
  ) where


import Halo2.Prelude
import Halo2.Types.EqualityConstraint


newtype EqualityConstraints = EqualityConstraints
  { getEqualityConstraints :: [EqualityConstraint] }
  deriving (Eq, Ord, Generic, Show)
