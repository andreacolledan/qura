module Main (main) where

import Control.Monad (when)
import Options.Applicative
import PrettyPrinter
import Solving.CVC5
import Parser(runParser)
import Text.Megaparsec.Error
import Lang.Library.Prelude
import Index.Semantics.Global.Resource
import Index.Semantics.Local.Resource
import Index.Semantics.Global.Width
import Index.Semantics.Global.Qubits
import Index.Semantics.Global.TCount
import Index.Semantics.Global.Bits
import Index.Semantics.Global.GateCount
import Data.Maybe (isJust)
import Index.Semantics.Local.Depth
import Index.Semantics.Local.TDepth
import Lang.Module.AST
import Lang.Module.Parse
import Lang.Analysis
import Lang.Expr.AST (VariableId)
import System.IO.Extra
import System.Console.ANSI

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
  opts <- execParser interface
  mod <- parseSource opts
  libs <- getLibs opts
  outcome <- analyzeModule mod libs opts
  case outcome of
    Left err -> outputError err
    Right bindings -> outputBindings bindings

parseSource :: Arguments -> IO Module
parseSource CommandLineArguments{verbose=verb, filepath=file, grs=mgrs, lrs=mlrs} = do
  when verb $ putStrLn $ "Parsing " ++ file ++ "..."
  source <- readFile file
  case runParser parseModule (isJust mgrs) (isJust mlrs) file source of
    Left err -> error $ errorBundlePretty err
    Right mod -> return mod

getLibs :: Arguments -> IO [Module]
getLibs CommandLineArguments{noprelude = nopre} = return $ ([prelude | not nopre]) -- for now, we only allow the prelude as a library

analyzeModule :: Module -> [Module] -> Arguments -> IO (Either TypeError [(VariableId, Type)])
analyzeModule
  mod libs CommandLineArguments{verbose=verb, debug=deb, grs=mgrs, lrs=mlrs} = do
    when verb $ do
      putStrLn $ "Parsed successfully as \n\t" ++ pretty mod
      putStrLn "Inferring type..."
    withSolver deb $ \qfh -> runAnalysis mod libs qfh mgrs mlrs


outputError :: (Show a) => a -> IO ()
outputError e = do
  hSetSGR stderr [SetColor Foreground Vivid Red]
  hPrint stderr e
  hSetSGR stderr [Reset]

outputBindings :: [(VariableId, Type)] -> IO ()
outputBindings = putStrLn . concatMap (\(id, typ) -> id ++ " :: " ++ pretty typ ++ "\n")
