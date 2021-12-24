{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.LookupArguments
  ( LookupArguments (LookupArguments, getLookupArguments)
  ) where


import Halo2.Prelude
import Halo2.Types.LookupArgument (LookupArgument)


newtype LookupArguments = LookupArguments { getLookupArguments :: [LookupArgument] }
  deriving (Eq, Ord, Show, Generic)
