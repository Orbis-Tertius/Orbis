{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Halo2.Types.InputExpression ( InputExpression (InputExpression, getInputExpression) ) where


import Halo2.Prelude

import Halo2.Types.Polynomial (Polynomial)


newtype InputExpression = InputExpression { getInputExpression :: Polynomial }
  deriving (Eq, Ord, Show, Generic)
