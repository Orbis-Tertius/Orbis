{-# LANGUAGE NoImplicitPrelude #-}


module UPLC2C.Command ( compileFile ) where


import Codec.Serialise (deserialiseOrFail)
import Data.ByteString.Lazy (readFile)
import Data.Text.IO (writeFile)
import Plutus.V1.Ledger.Scripts (Script (..))
import qualified UntypedPlutusCore.Core.Type as UPLC

import UPLC2C.Compile (compile)
import UPLC2C.Prelude
import UPLC2C.Types.CCode (CCode (..))
import UPLC2C.Types.InputFilePath (InputFilePath (..))
import UPLC2C.Types.OutputFilePath (OutputFilePath (..))


compileFile :: MonadIO m => InputFilePath -> OutputFilePath -> m ()
compileFile (InputFilePath inFilePath) (OutputFilePath outFilePath) = do
  inFileBytes <- liftIO $ readFile inFilePath
  case deserialiseOrFail inFileBytes of
    Right (Script (UPLC.Program _ _ term)) -> do
      CCode objectCode <- compile term
      liftIO $ writeFile outFilePath objectCode
    Left _ ->
      liftIO $ putStrLn "input is not a valid UPLC CBOR representation"
