{-# LANGUAGE NoImplicitPrelude #-}


module UPLC2C.Command ( compileFile ) where


import UPLC2C.Prelude
import UPLC2C.Types.InputFilePath (InputFilePath (..))
import UPLC2C.Types.OutputFilePath (OutputFilePath (..))


compileFile :: MonadIO m => InputFilePath -> OutputFilePath -> m ()
compileFile _ _ = liftIO todo


todo :: a
todo = todo
