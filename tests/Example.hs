{-# LANGUAGE DeriveGeneric #-}
module Example where
import Data.Yaml.Union
import Data.Yaml
import Data.Maybe


readEx :: [FilePath] -> IO Object
readEx fs = fmap fromJust (decodeFiles fs)

main :: IO ()
main =
  do en <- readEx ["example/en.yaml"]
     de <- readEx ["example/en.yaml", "example/de.yaml"]
     print en
     print de
