module Main (main) where

import Control.Monad (when)
import Index.Semantics
import Lang.Type.Semantics (simplifyType)
import Lang.Analysis.Analyze
import Lang.Expr.Parse
import Options.Applicative
import PrettyPrinter
import Solving.CVC5
import System.Console.ANSI
import System.IO.Extra
import Text.Parsec (parse)

data Arguments = CommandLineArguments
  { filepath :: String,
    verbose :: Bool,
    debug :: Maybe String
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

main :: IO ()
main = do
  CommandLineArguments {filepath = file, verbose = verb, debug = deb} <- execParser interface
  when verb $ putStrLn $ "Parsing " ++ file ++ "..."
  contents <- readFile file
  case parse parseProgram "" contents of
    Left err -> print err
    Right ast -> do
      when verb $ do
        putStrLn $ "Parsed successfully as \n\t" ++ pretty ast
        putStrLn "Inferring type..."
      withSolver deb $ \qfh -> do
        outcome <- runAnalysis ast qfh
        case outcome of
          Left err -> do
            hSetSGR stderr [SetColor Foreground Vivid Red]
            hPrint stderr err
            hSetSGR stderr [Reset]
          Right (t, i) -> do
            t' <- simplifyType qfh t
            i' <- simplifyIndexStrong qfh i
            putStrLn $ "* Inferred type: " ++ pretty t'
            putStrLn $ "* Inferred bound: " ++ pretty i'
