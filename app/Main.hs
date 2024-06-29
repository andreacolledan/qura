module Main (main) where

import Control.Monad (when)
import Index.Semantics
import Lang.Type.Semantics
import Lang.Analysis.Analyze
import Lang.Expr.Parse
import Options.Applicative
import PrettyPrinter
import Solving.CVC5
import System.Console.ANSI
import System.IO.Extra
import Text.Parsec (runParser)
import Lang.Library.Prelude
import Index.Semantics.Width (widthResourceSemantics)
import Index.Semantics.GateCount (gateCountResourceSemantics)
import Index.Semantics.Resource (GlobalResourceSemantics, LocalResourceSemantics)
import Data.Maybe (fromMaybe, isJust, fromJust)
import qualified Index.Parse as IP

globalResourceArgParser :: ReadM GlobalResourceSemantics
globalResourceArgParser = do
  s <- str 
  case s of
    "width" -> return widthResourceSemantics
    "gatecount" -> return gateCountResourceSemantics
    _ -> readerError "Supported global resources are 'width' and 'gatecount'."

localResourceArgParser :: ReadM LocalResourceSemantics
localResourceArgParser = readerError "Local resources are unsupported."

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
        <> progDesc "Verify the resource consumption of a quantum circuit description program"
        <> header "QuRA: a static analysis tool for the verification of the resource consumption of quantum programs"
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
        outcome <- runAnalysis actualAst qfh mgrs mlrs
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
