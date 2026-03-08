module Main (main) where

import Analyzer (runAnalysis)
import Control.Monad (when, unless)
import Data.List (intercalate)
import Data.Maybe (catMaybes, isJust, fromMaybe)
import Interface (CLArguments (..), cliInterface)
import Interpreter (Configuration (..), InterpreterResult (..), runInterpreter)
import Options.Applicative (execParser)
import PQ (Module, prelude, toTypeBindings)
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
import System.Directory.Internal.Prelude (exitFailure, stdout, IOMode (WriteMode))
import System.IO.Extra (hPutStrLn, stderr)
import Text.Pretty.Simple (pPrint)
import GHC.IO.Handle.FD (withFile)

main :: IO ()
main = do
  ensureCVC5
  opts <- parseCLArguments
  mod <- parseSource opts
  libs <- getLibs opts
  analyzedModule <- analyzeModule mod libs opts
  interpretModule analyzedModule libs opts

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
        putStrLn "Parsed the following AST: \n\t"
        pPrint mod
        putStrLn ""
      return mod

getLibs :: CLArguments -> IO [Module]
getLibs CommandLineArguments {noprelude = nopre} = return ([prelude | not nopre]) -- for now, we only allow the prelude as a library

analyzeModule :: Module -> [Module] -> CLArguments -> IO Module
analyzeModule mod libs CommandLineArguments {filepath = fp, verbose = verb, debug = deb, grs = mgrs, lrs = mlrs} = do
  when verb $ putStrLn $ "Type-checking '" ++ fp ++ "'..."
  outcome <- withSolver deb $ \qfh -> runAnalysis mod libs qfh mgrs mlrs
  case outcome of
    Left err -> abortWithMessage $ show err
    Right analyzedModule -> do
      when verb $ do
        let metrics = catMaybes [pretty <$> mgrs, pretty <$> mlrs]
        putStrLn $ "Checked " ++ intercalate ", " ("type" : metrics) ++ ". Top level bindings:\n"
        putStrLn $ concatMap (\(id, typ) -> id ++ " :: " ++ pretty typ ++ "\n") $ toTypeBindings analyzedModule
      return analyzedModule


interpretModule :: Module -> [Module] -> CLArguments -> IO ()
interpretModule mod libs opts@CommandLineArguments {verbose = verb, norun = nr, filepath = fp, outputFilepath = ofp} = do
  unless nr $ do
    when verb $ putStrLn $ "Running " ++ fp ++ "..."
    case runInterpreter mod libs opts of
      Left err -> abortWithMessage $ show err
      Right intResult -> do
        when verb $ do
          let config = cfg intResult
          putStrLn $ "File '" ++ fp ++ "' produced the following circuit IR:\n"
          putStrLn (pretty (circuit config))
          putStrLn "\nWhile evaluating to:\n"
          putStrLn $ pretty (term config) ++ "\n"
          putStrLn $ "Writing circuit to " ++ fromMaybe "stdout" ofp ++ "...\n"
        let outputString = pretty (qasm intResult)
        case ofp of
          Just jofp -> writeFile jofp outputString
          Nothing -> putStr outputString

abortWithMessage :: String -> IO a
abortWithMessage e = do
  hSetSGR stderr [SetColor Foreground Vivid Red]
  hPutStrLn stderr e
  hSetSGR stderr [Reset]
  exitFailure