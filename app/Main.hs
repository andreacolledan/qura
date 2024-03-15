module Main (main) where

import Control.Monad (when)
import Index.Semantics
import Lang.Type.Semantics (simplifyType)
import Lang.Unified.Infer
import qualified Lang.Unified.Parse as U
import PrettyPrinter
import System.Console.ArgParser
import Text.Parsec
import System.Console.ANSI
import System.IO.Extra
import Solving.CVC5

data CommandLineArguments = CommandLineArguments
  { filepath :: String,
    paper :: Bool,
    verbose :: Bool
  }

commandLineParser :: ParserSpec CommandLineArguments
commandLineParser =
  CommandLineArguments
    `parsedBy` reqPos "filepath"
    `Descr` "The file to parse"
    `andBy` boolFlag "paper"
    `Descr` "Use the original syntax from the paper"
    `andBy` boolFlag "verbose"
    `Descr` "Print verbose output (parser output)"

main :: IO ()
main = withParseResult commandLineParser $ \args -> do
  let CommandLineArguments {filepath = file, paper = p, verbose = verb} = args
  when verb $ putStrLn $ "Parsing " ++ file ++ "..."
  contents <- readFile file
  case parse U.parseProgram "" contents of
    Left err -> print err
    Right ast -> do
      when verb $ do
        putStrLn $ "Parsed successfully as \n\t" ++ pretty ast
        putStrLn "Inferring type..."
      withQueryFile file $ \qfh -> do
        let outcome = runTypeInference ast qfh
        case outcome of
          Left err -> do
            hSetSGR stderr [SetColor Foreground Vivid Red]
            hPrint stderr err
            hSetSGR stderr [Reset]
          Right (t, i) -> do
            putStrLn $ "* Inferred type: " ++ pretty (simplifyType qfh t)
            putStrLn $ "* Inferred bound: " ++ pretty (simplifyIndex qfh i)
