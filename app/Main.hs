module Main (main) where

import Control.Monad (when)
import Index.Semantics
import Lang.Type.Semantics
import Lang.Analysis.InferRefinedType
import Lang.Expr.Parse
import Options.Applicative
import PrettyPrinter
import Solving.CVC5
import System.Console.ANSI
import System.IO.Extra
import Parser(runParser, ParserConfig(..))
import Text.Megaparsec.Error
import Lang.Library.Prelude
import Index.Semantics.Global.Resource
import Index.Semantics.Local.Resource
import Index.Semantics.Global.Width
import Index.Semantics.Global.Qubits
import Index.Semantics.Global.TCount
import Index.Semantics.Global.Bits
import Index.Semantics.Global.GateCount
import Data.Maybe (isJust, fromJust)
import Index.Semantics.Local.Depth
import Index.Semantics.Local.TDepth

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

data Arguments = CommandLineArguments
  { filepath :: String,
    verbose :: Bool,
    debug :: Maybe String,
    noprelude :: Bool,
    grs :: Maybe GlobalMetricModule,
    lrs :: Maybe LocalMetricModule
  }

interface :: ParserInfo Arguments
interface =
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

main :: IO ()
main = do
  CommandLineArguments {
    filepath = file,
    verbose = verb,
    debug = deb,
    noprelude = nopre,
    grs = mgrs,
    lrs = mlrs} <- execParser interface
  when verb $ putStrLn $ "Parsing " ++ file ++ "..."
  contents <- readFile file
  case runParser ParserConfig{parsegra = isJust mgrs, parselra = isJust mlrs} parseMain file contents of
    Left err -> do
      hSetSGR stderr [SetColor Foreground Vivid Red]
      hPutStr stderr $ errorBundlePretty err
      hSetSGR stderr [Reset]
    Right ast -> do
      when verb $ do
        putStrLn $ "Parsed successfully as \n\t" ++ pretty ast
        putStrLn "Inferring type..."
      withSolver deb $ \qfh -> do
        let actualAst = if nopre then ast else library ast
        outcome <- runRefinedTypeInference actualAst qfh mgrs mlrs
        case outcome of
          Left err -> do
            hSetSGR stderr [SetColor Foreground Vivid Red]
            hPrint stderr err
            hSetSGR stderr [Reset]
          Right (t, i) -> do
            t' <- simplifyType qfh mgrs mlrs t
            putStrLn $ "* Inferred type: " ++ pretty t'
            when (isJust mgrs) $ do
              i' <- simplifyIndex qfh mgrs mlrs (fromJust i)
              putStrLn $ "* Inferred " ++ pretty mgrs ++ " upper bound: " ++ pretty i'
