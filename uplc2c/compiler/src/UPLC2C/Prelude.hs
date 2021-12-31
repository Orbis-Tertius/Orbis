{-# LANGUAGE NoImplicitPrelude #-}


module UPLC2C.Prelude
  ( module Prelude
  , module Control.Monad.IO.Class
  , module Data.Map
  , module Data.Set
  , module Data.Text
  ) where


import Prelude hiding (readFile, writeFile)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
