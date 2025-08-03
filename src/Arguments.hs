module Arguments (
    Arguments(..),
    cliInterface
) where

import Metric
import Options.Applicative

data Arguments = CommandLineArguments
  { filepath :: String,
    verbose :: Bool,
    debug :: Maybe String,
    noprelude :: Bool,
    grs :: Maybe GlobalMetricModule,
    lrs :: Maybe LocalMetricModule
  }

globalMetricArgParser :: ReadM GlobalMetricModule
globalMetricArgParser = do
  s <- str
  case s of
    "width" -> return widthMetric
    "qubits" -> return qubitsMetric
    "bits" -> return bitsMetric
    "gatecount" -> return gateCountMetric
    "tcount" -> return tCountMetric
    _ -> readerError "Supported global resources are 'width', 'gatecount', 'qubits', 'bits', 'tcount'."

localMetricArgParser :: ReadM LocalMetricModule
localMetricArgParser = do
  s <- str
  case s of
    "depth" -> return depthMetric
    "tdepth" -> return tDepthMetric
    _ -> readerError "Supported local resources are 'depth', `tdepth`."

cliInterface :: ParserInfo Arguments
cliInterface =
  info
    (arguments <**> helper)
    ( fullDesc
        <> progDesc "Verify the resource consumption of the program in FILE according to the chosen METRIC."
        <> header "QuRA: a static analysis tool for the resource verification of quantum circuit description programs"
    )
  where
    arguments :: Parser Arguments
    arguments =
      CommandLineArguments
        <$> strArgument
          ( metavar "FILE"
              <> help "The file to type-check and analyze"
          )
        <*> switch
          ( long "verbose"
              <> short 'v'
              <> help "Print verbose output"
          )
        <*> optional (strOption
          ( long "debug"
              <> short 'd'
              <> metavar "DEBUG"
              <> help "Print SMT queries to file DEBUG"
          ))
        <*> switch
          ( long "noprelude"
              <> help "Do not include the prelude"
          )
        <*> optional (option globalMetricArgParser
          ( long "global-metric-analysis"
              <> short 'g'
              <> metavar "METRIC"
              <> help "Analyse global METRIC"
              ))
        <*> optional (option localMetricArgParser
          ( long "local-metric-analysis"
              <> short 'l'
              <> metavar "METRIC"
              <> help "Analyse local METRIC"
              ))