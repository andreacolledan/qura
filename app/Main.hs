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
import Text.Parsec (runParser)
import Lang.Library.Prelude
import Index.Semantics.Resource (GlobalResourceSemantics, LocalResourceSemantics)
import Index.Semantics.Width (widthResourceSemantics)
import Index.Semantics.Qubits (qubitsResourceSemantics)
import Index.Semantics.TCount (tCountResourceSemantics)
import Index.Semantics.Bits (bitsResourceSemantics)
import Index.Semantics.GateCount (gateCountResourceSemantics)
import Data.Maybe (isJust, fromJust)
import qualified Index.Parse as IP
import Index.Semantics.Depth

globalResourceArgParser :: ReadM GlobalResourceSemantics
globalResourceArgParser = do
  s <- str 
  case s of
    "width" -> return widthResourceSemantics
    "qubits" -> return qubitsResourceSemantics
    "bits" -> return bitsResourceSemantics
    "gatecount" -> return gateCountResourceSemantics
    "tcount" -> return tCountResourceSemantics
    _ -> readerError "Supported global resources are 'width', 'gatecount', 'qubits', 'bits', 'tcount'."

localResourceArgParser :: ReadM LocalResourceSemantics
localResourceArgParser = do
  s <- str
  case s of
    "depth" -> return depthResourceSemantics
    _ -> readerError "Supported local resources are 'depth'."

data Arguments = CommandLineArguments
  { filepath :: String,
    verbose :: Bool,
    debug :: Maybe String,
    noprelude :: Bool,
    grs :: Maybe GlobalResourceSemantics,
    lrs :: Maybe LocalResourceSemantics
  }

interface :: ParserInfo Arguments
interface =
  info
    (arguments <**> helper)
    ( fullDesc
        <> progDesc "Verify the resource consumption of the program in FILE according to RESOURCE."
        <> header "QuRA: a static analysis tool for the resource verification of quantum circuit description programs"
    )
  where
    arguments :: Parser Arguments
    arguments =
      CommandLineArguments
        <$> strArgument
          ( metavar "FILE"
              <> help "The file to type-check"
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
        <*> optional (option globalResourceArgParser
          ( long "global-resource-analysis"
              <> short 'g'
              <> metavar "RESOURCE"
              <> help "Analyse global RESOURCE"
              ))
        <*> optional (option localResourceArgParser
          ( long "local-resource-analysis"
              <> short 'l'
              <> metavar "RESOURCE"
              <> help "Analyse local RESOURCE"
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
  case runParser parseProgram IP.ParserConfig{IP.parsegra = isJust mgrs, IP.parselra = isJust mlrs} "" contents of
    Left err -> print err
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
              putStrLn $ "* Inferred bound: " ++ pretty i'
