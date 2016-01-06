module Main where
import Options.Applicative
import qualified Data.Yaml as Y
import Data.Yaml.Pretty
import Data.Yaml.Union
import qualified Data.ByteString.Char8 as B

data Options =
  Options { --variables :: String
           dashes :: Bool
          ,files :: [String]}

cfg :: Parser Options
cfg =
  Options <$>
  -- strOption (short 'V' <> long "variable" <> metavar "VAR-DEF" <>
  --              help "Set variables of the yaml-file \
  --               \ (concatenation with ':', nesting with '.', eg 'lang=de:dict.blue=blau').") <*>
  switch (short 'd' <> long "dashes" <> help "Whether to print dashes at beginning and end" ) <*>
  some (argument str (metavar "YAML-FILES"))

union :: Options -> IO ()
union (Options{files = fs, dashes = dsh}) =
  do s <- decodeFilesEither fs
     let yaml =
           case s of
             Left s -> error $ "Yaml-File parsing failed " ++ s
             Right x ->  x :: Y.Object
     let ymlStr = encodePretty defConfig yaml
     let str = if dsh
                  then B.unlines [ B.pack "---",  ymlStr, B.pack "..."]
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
