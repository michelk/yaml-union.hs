{-# LANGUAGE DeriveGeneric #-}
module Example where
import Data.Yaml.Override
import Data.Yaml
import Data.Maybe
import GHC.Generics

data Example = Example { title :: String
                        ,author :: String
                        ,description :: Desc
                       }
               deriving (Show, Generic)
instance FromJSON Example

data Desc =
  Desc {abstract :: String
       ,file :: FilePath
       ,image :: FilePath}
               deriving (Show, Generic)

instance FromJSON Desc

readEx :: [FilePath] -> IO Example
readEx fs = fmap fromJust (decodeFiles fs)

main :: IO ()
main =
  do en <- readEx ["example/en.yaml"]
     de <- readEx ["example/en.yaml", "example/de.yaml"]
     print en
     print de
