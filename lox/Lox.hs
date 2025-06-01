module Lox where

import qualified Data.ByteString as B
import qualified Interpreter
import System.Console.ANSI (clearScreen)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.IO.Error (tryIOError)

main :: IO ()
main = do
  a <- getArgs
  if length a > 1
    then
      putStrLn "Usage: jlox [script]" >> exitWithErrorMessage "hey" (ExitFailure 64)
    else
      if length a == 1
        then runFile (head a)
        else runPrompt

runPrompt :: IO ()
runPrompt = do
  putStr "> "
  hFlush stdout
  result <- tryIOError getLine
  case result of
    Right "clear" -> clearScreen >> runPrompt
    Right l -> run l >> runPrompt
    Left _ -> putStrLn "Goodbye."

runFile :: FilePath -> IO ()
runFile c = do
  content <- B.readFile c
  --   run $ toString content
  pure ()

run :: String -> IO ()
run = Interpreter.eval'

error :: Int -> String -> IO ()
error l = report l ""

report :: Int -> String -> String -> IO ()
report l loc msg = hPutStrLn stderr $ "[line " <> show l <> "] Error" <> loc <> ": " <> msg

exitWithErrorMessage :: String -> ExitCode -> IO a
exitWithErrorMessage str e = hPutStrLn stderr str >> exitWith e