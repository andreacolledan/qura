module Main (main) where

import Analyzer (runAnalysis)
import Control.Monad (when)
import Data.List (intercalate)
import Data.Maybe (catMaybes, isJust)
import Interface (CLArguments (..), cliInterface)
import Interpreter
import Options.Applicative (execParser)
import PQ (Module, prelude)
import Parser (errorBundlePretty, parseModule, runParser)
import PrettyPrinter (Pretty (pretty))
import Solver (withSolver)
import System.Console.ANSI
  ( Color (Red),
    ColorIntensity (Vivid),
    ConsoleLayer (Foreground),
    SGR (Reset, SetColor),
    hSetSGR,
  )
import System.Directory (findExecutable)
import System.Directory.Internal.Prelude (exitFailure)
import System.IO.Extra (hPutStrLn, stderr)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  ensureCVC5
  opts <- parseCLArguments
  mod <- parseSource opts
  libs <- getLibs opts
  analyzeModule mod libs opts
  interpretModule mod libs opts

ensureCVC5 :: IO ()
ensureCVC5 = do
  mpath <- findExecutable "cvc5"
  case mpath of
    Nothing ->
      abortWithMessage $
        unlines
          [ "Error: cvc5 is not installed or not in PATH.",
            "To install cvc5, follow instructions at https://cvc5.github.io/"
          ]
    Just _ -> return ()

parseCLArguments :: IO CLArguments
parseCLArguments = execParser cliInterface

parseSource :: CLArguments -> IO Module
parseSource CommandLineArguments {verbose = verb, filepath = file, grs = mgrs, lrs = mlrs} = do
  when verb $ putStrLn $ "Parsing " ++ file ++ "..."
  source <- readFile file
  case runParser parseModule (isJust mgrs) (isJust mlrs) file source of
    Left err -> error $ errorBundlePretty err
    Right mod -> do
      when verb $ do
        putStrLn "Abstract Syntax Tree: \n\t"
        pPrint mod
      return mod

getLibs :: CLArguments -> IO [Module]
getLibs CommandLineArguments {noprelude = nopre} = return ([prelude | not nopre]) -- for now, we only allow the prelude as a library

analyzeModule :: Module -> [Module] -> CLArguments -> IO ()
analyzeModule mod libs CommandLineArguments {filepath = fp, verbose = verb, debug = deb, grs = mgrs, lrs = mlrs} = do
  when verb $ putStrLn $ "Inferring type for '" ++ fp ++ "'..."
  outcome <- withSolver deb $ \qfh -> runAnalysis mod libs qfh mgrs mlrs
  case outcome of
    Left err -> abortWithMessage $ show err
    Right bindings -> do
      putStrLn $ "Analyzed file '" ++ fp ++ "'."
      let metrics = catMaybes [pretty <$> mgrs, pretty <$> mlrs]
      putStrLn $ "Checked " ++ intercalate ", " ("type" : metrics) ++ ".\n"
      putStrLn $ concatMap (\(id, typ) -> id ++ " :: " ++ pretty typ ++ "\n\n") bindings

interpretModule :: Module -> [Module] -> CLArguments -> IO ()
interpretModule mod libs CommandLineArguments {verbose = verb, noprelude = _noprelude, filepath = fp, debug = _debug} = do
  when verb $ putStrLn $ "Interpreting " ++ fp ++ "..."
  case runInterpreter mod libs of
    Left err -> abortWithMessage $ show err
    Right config -> do
      putStrLn $ "File '" ++ fp ++ "' produced circuit:\n"
      print $ circuit config
      putStrLn "\nwhile evaluating to:\n"
      print $ term config

abortWithMessage :: String -> IO ()
abortWithMessage e = do
  hSetSGR stderr [SetColor Foreground Vivid Red]
  hPutStrLn stderr e
  hSetSGR stderr [Reset]
  exitFailure