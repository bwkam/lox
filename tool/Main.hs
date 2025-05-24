module Main where

import Control.Monad (when)
import GHC.IO.Exception (ExitCode (ExitFailure))
import System.Environment (getArgs)
import System.Exit (exitWith)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) (exitWithErrorMessage "Usage: print_ast <ast>" (ExitFailure 64))
  let ast = head args
  pure ()

exitWithErrorMessage :: String -> ExitCode -> IO a
exitWithErrorMessage str e = hPutStrLn stderr str >> exitWith e