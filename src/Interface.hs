module Interface (
    CLArguments(..),
    cliInterface
) where

import Data.Version (showVersion)
import Metric.Global (GlobalMetricModule)
import Metric.Global.Bits (bitsMetric)
import Metric.Global.GateCount (gateCountMetric)
import Metric.Global.QasmGateCount (qasmGateCountMetric)
import Metric.Global.QasmWidth (qasmWidthMetric)
import Metric.Global.Qubits (qubitsMetric)
import Metric.Global.TCount (tCountMetric)
import Metric.Global.Width (widthMetric)
import Metric.Local (LocalMetricModule)
import Metric.Local.Depth (depthMetric)
import Metric.Local.QasmDepth (qasmDepthMetric)
import Metric.Local.TDepth (tDepthMetric)
import Options.Applicative
  ( Parser,
    ParserInfo,
    ReadM,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    optional,
    progDesc,
    readerError,
    short,
    simpleVersioner,
    str,
    strArgument,
    strOption,
    switch,
    (<**>),
  )
import Paths_qura (version)

data CLArguments = CommandLineArguments
  { filepath :: String,
    outputFilepath :: Maybe String,
    verbose :: Bool,
    norun :: Bool,
    debug :: Maybe String,
    noprelude :: Bool,
    grs :: Maybe GlobalMetricModule,
    lrs :: Maybe LocalMetricModule,
    qubitRecycling :: Bool
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
    -- qasm
    "qasmwidth" -> return qasmWidthMetric
    "qasmgatecount" -> return qasmGateCountMetric
    _ -> readerError "Supported global resources are 'width', 'gatecount', 'qubits', 'bits', 'tcount','qasmwidth', 'qasmgatecount'."

localMetricArgParser :: ReadM LocalMetricModule
localMetricArgParser = do
  s <- str
  case s of
    "depth" -> return depthMetric
    "tdepth" -> return tDepthMetric
    -- qasm
    "qasmdepth" -> return qasmDepthMetric
    _ -> readerError "Supported local resources are 'depth', 'tdepth', 'qasmdepth'."

cliInterface :: ParserInfo CLArguments
cliInterface =
  info
    (arguments <**> helper <**> simpleVersioner ("QuRA version " ++ showVersion Paths_qura.version))
    ( fullDesc
        <> progDesc "Verify the resource consumption of the program FILE according to the chosen METRIC and run it to produce a circuit"
        <> header "QuRA: a tool for resource-aware quantum programming"
    )
  where
    arguments :: Parser CLArguments
    arguments =
      CommandLineArguments
        <$> strArgument
          ( metavar "FILE"
              <> help "The file to type-check and run"
          )
        <*> optional ( strOption
          ( long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "Place the output circuit into FILE"
          )
        )
        <*> switch
          ( long "verbose"
              <> short 'v'
              <> help "Print verbose output"
          )
        <*> switch
          ( long "no-run"
              <> help "Type-check only, without running the program"
          )
        <*> optional (strOption
          ( long "debug"
              <> short 'd'
              <> metavar "DEBUG"
              <> help "Print SMT queries to file DEBUG"
          ))
        <*> switch
          ( long "no-prelude"
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
        <*> switch
          ( long "no-recycling"
              <> help "Do not recycle discarded qubits during initializations"
          )
