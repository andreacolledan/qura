module Main (main) where

import Analyzer
import Control.Monad (when)
import Data.List (intercalate)
import Data.Maybe (catMaybes, isJust)
import Options.Applicative
import PQ
import Parser
import PrettyPrinter
import Solver
import System.Console.ANSI
import System.IO.Extra
import Text.Pretty.Simple (pPrint)
import Arguments
import System.Directory (findExecutable)

main :: IO ()
main = do
  ensureCVC5
  opts <- parseCLArguments
  mod <- parseSource opts
  libs <- getLibs opts
  outcome <- analyzeModule mod libs opts
  case outcome of
    Left err -> outputError err
    Right bindings -> outputBindings opts bindings

ensureCVC5 :: IO ()
ensureCVC5 = do
  mpath <- findExecutable "cvc5"
  case mpath of
    Nothing -> error $ unlines [
      "Error: cvc5 is not installed or not in PATH.\n",
      "To install cvc5, follow instructions at https://cvc5.github.io/"]
    Just _  -> return ()

parseCLArguments :: IO Arguments
parseCLArguments = execParser cliInterface

parseSource :: Arguments -> IO Module
parseSource CommandLineArguments{verbose=verb, filepath=file, grs=mgrs, lrs=mlrs} = do
  when verb $ putStrLn $ "Parsing " ++ file ++ "..."
  source <- readFile file
  case runParser parseModule (isJust mgrs) (isJust mlrs) file source of
    Left err -> error $ errorBundlePretty err
    Right mod -> do
      when verb $ do
        putStrLn $ "Abstract Syntax Tree: \n\t"
        pPrint mod
      return mod

getLibs :: Arguments -> IO [Module]
getLibs CommandLineArguments{noprelude = nopre} = return $ ([prelude | not nopre]) -- for now, we only allow the prelude as a library

analyzeModule :: Module -> [Module] -> Arguments -> IO (Either TypeError [(VariableId, Type)])
analyzeModule
  mod libs CommandLineArguments{verbose=verb, debug=deb, grs=mgrs, lrs=mlrs} = do
    when verb $ do
      putStrLn "Inferring type..."
    withSolver deb $ \qfh -> runAnalysis mod libs qfh mgrs mlrs


outputError :: (Show a) => a -> IO ()
outputError e = do
  hSetSGR stderr [SetColor Foreground Vivid Red]
  hPrint stderr e
  hSetSGR stderr [Reset]

outputBindings :: Arguments -> [(VariableId, Type)] -> IO ()
outputBindings opts bindings = do
  putStrLn $ "Analyzing file '" ++ filepath opts ++ "'."
  let metrics = catMaybes [pretty <$> grs opts, pretty <$> lrs opts]
  putStrLn $ "Checked " ++ intercalate ", " ("type" : metrics) ++ ".\n"
  putStrLn $ concatMap (\(id, typ) -> id ++ " :: " ++ pretty typ ++ "\n\n") bindings
