{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module UPLC2C.Types.CProgramBuilderT ( CProgramBuilderT (..), run ) where


import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Trans.State (StateT (..), get, put, evalStateT)
import Text.StringRandom (stringRandomIO)

import UPLC2C.Prelude
import UPLC2C.Types.CName (CName (..))
import UPLC2C.Types.CProgram (CProgram (..))
import UPLC2C.Types.CProgramBuilder (CProgramBuilder (..))


newtype CProgramBuilderT m a = CProgramBuilderT
  { unCProgramBuilderT :: StateT BuilderState m a }
  deriving (Functor, Applicative, Monad)


data BuilderState =
  BuilderState
  { program :: CProgram
  , usedNames :: Set CName
  }


instance MonadIO m => CProgramBuilder (CProgramBuilderT m) where
  getProgram = CProgramBuilderT $ program <$> get
  addToProgram name def = do
    s <- CProgramBuilderT $ get
    let s' = s { program = CProgram $ Map.insert name def (unCProgram $ program s) }
    CProgramBuilderT $ put s'
  genSym = do
    s     <- CProgramBuilderT $ get
    fresh <- CProgramBuilderT $ genNameNotIn (usedNames s)
    let s' = s { usedNames = Set.insert fresh (usedNames s) }
    CProgramBuilderT $ put s'
    return fresh


run :: Monad m => CProgramBuilderT m a -> m a
run = flip evalStateT initialState . unCProgramBuilderT


initialState :: BuilderState
initialState = BuilderState (CProgram mempty) mempty


-- Generates a random name (not guaranteed not to conflict).
genRandomName :: MonadIO m => m CName
genRandomName = CName <$> liftIO (stringRandomIO "g__[a-zA-Z][a-zA-Z0-9]")


-- Generates a name not in the given set.
genNameNotIn :: MonadIO m => Set CName -> m CName
genNameNotIn s = do
  n <- genRandomName
  if n `Set.member` s
    then genNameNotIn s
    else return n
