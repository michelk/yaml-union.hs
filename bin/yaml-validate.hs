import Prelude hiding (getContents)

import Data.ByteString (getContents)
import System.Environment (getArgs)
import System.Exit

import Data.Yaml (Value, decodeFileEither, decodeEither')

helpMessage :: IO ()
helpMessage = putStrLn "Usage: yaml-validate FILE\nUse - as FILE to indicate stdin" >> exitFailure

validate :: Show a => String -> Either a Value -> IO b
validate f ejson =
  case ejson of
    Left err -> do
      putStrLn ("Error parsing " ++ f ++ "\n")
      print err
      exitFailure
    Right _ -> exitSuccess

main :: IO ()
main = do
    args <- getArgs
    case args of
       -- strict getContents will read in all of stdin at once
       (["-h"]) -> helpMessage
       (["--help"]) -> helpMessage
       (["-"]) -> getContents >>= (validate "-") . decodeEither'
       ([f])   -> decodeFileEither f >>= validate f
       _ -> helpMessage

