{-# LANGUAGE NoImplicitPrelude #-}


module UPLC2C.Command ( main ) where


import Codec.Serialise (deserialiseOrFail)
import Data.ByteString.Lazy (readFile)
import Data.Text.IO (writeFile)
import qualified Options.Applicative as O
import Plutus.V1.Ledger.Scripts (Script (..))
import qualified UntypedPlutusCore.Core.Type as UPLC

import UPLC2C.Compile (compile)
import UPLC2C.Prelude
import UPLC2C.Types.CCode (CCode (..))
import UPLC2C.Types.InputFilePath (InputFilePath (..))
import UPLC2C.Types.OutputFilePath (OutputFilePath (..))


data Command = CompileFile InputFilePath OutputFilePath


main :: MonadIO m => m ()
main = runCommand =<< parseCommand


parseCommand :: MonadIO m => m Command
parseCommand = liftIO $ O.execParser commandInfo


inputFilePath :: O.Parser InputFilePath
inputFilePath =
  O.argument (InputFilePath <$> O.str) (O.metavar "INPUT" <> O.help "The input file path")


outputFilePath :: O.Parser OutputFilePath
outputFilePath =
  O.argument (OutputFilePath <$> O.str) (O.metavar "OUTPUT" <> O.help "The output file path")


command :: O.Parser Command
command = CompileFile <$> inputFilePath <*> outputFilePath


commandInfo :: O.ParserInfo Command
commandInfo =
  O.info (command O.<**> O.helper)
    ( O.fullDesc
   <> O.progDesc "Compile Plutus bytecode to C"
   <> O.header "uplc2c - Untyped Plutus Core to C"
    )


runCommand :: MonadIO m => Command -> m ()
runCommand (CompileFile inPath outPath) = compileFile inPath outPath


compileFile :: MonadIO m => InputFilePath -> OutputFilePath -> m ()
compileFile (InputFilePath inFilePath) (OutputFilePath outFilePath) = do
  inFileBytes <- liftIO $ readFile inFilePath
  case deserialiseOrFail inFileBytes of
    Right (Script (UPLC.Program _ _ term)) -> do
      CCode objectCode <- compile term
      liftIO $ writeFile outFilePath objectCode
    Left _ ->
      liftIO $ putStrLn "input is not a valid UPLC CBOR representation"
