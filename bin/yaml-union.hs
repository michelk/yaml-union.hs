module Main where
import Options.Applicative
import Data.Monoid
import qualified Data.Yaml as Y
import Data.Yaml.Pretty
import Data.Yaml.Union
import Data.Yaml.Dirs
import Control.Monad
import System.Directory
import qualified Data.ByteString.Char8 as B

data Options = Options --variables :: String
  { dirFile :: String
  , dashes :: Bool
  , recursive :: Bool
  , files :: [String]
  }

cfg :: Parser Options
cfg =
  Options <$>
  -- strOption (short 'V' <> long "variable" <> metavar "VAR-DEF" <>
  --              help "Set variables of the yaml-file \
  --               \ (concatenation with ':', nesting with '.', eg 'lang=de:dict.blue=blau').") <*>
  strOption (short 'f' <> long "dir-file" <> metavar "FILE" <> value "" <>
               help "File to union directories") <*>
  switch (short 'd' <> long "dashes" <> help "Whether to print dashes at beginning and end" ) <*>
  switch (short 'r' <> long "recursive" <> help "Unions dirs (Provide also 'dir-file')" ) <*>
  some (argument str (metavar "YAML-FILES"))

union :: Options -> IO ()
union (Options {files = fs, dashes = dsh, recursive = recu,dirFile = unionF}) = do
  when (length fs == 0) (error "Please provide at least one input file")
  yaml <-
    if recu
       then do unionDirs unionF fs
       else do s <- decodeFilesEither fs
               return
                 (case s of
                    Left s -> error $ "Yaml-File parsing failed " ++ s
                    Right x -> x)
  let ymlStr = encodePretty defConfig yaml
  let str =
        if dsh
          then B.unlines [B.pack "---", ymlStr, B.pack "..."]
          else ymlStr
  B.putStrLn str

main :: IO ()
main = execParser opts >>= union
  where
    opts = info (helper <*> cfg )
      ( fullDesc
     <> header  "Program to union yaml-files"
     <> progDesc "Union yaml files recursively from left to right"
         )
