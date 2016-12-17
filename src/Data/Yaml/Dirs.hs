{-# LANGUAGE OverloadedStrings #-}
module Data.Yaml.Dirs (mergeDirs, decodeFiles) where

import qualified Data.Yaml as Yaml
import           Data.Maybe (fromJust)
import           System.FilePath
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.List (sort)

mergeDirs :: FilePath -> FilePath -> IO Yaml.Object
mergeDirs _d _f = undefined

decodeFiles :: Bool -> [FilePath] -> IO Yaml.Value
decodeFiles sortKeysIncreasing fs =
  do vs <- mapM decodeFile (if sortKeysIncreasing then (sort fs) else (reverse . sort $ fs))
     return (Yaml.Array (V.fromList vs))

decodeFile :: FilePath -> IO Yaml.Value
decodeFile f =
  do yml' <- Yaml.decodeFileEither f
     let yml = case yml' of
           Left err -> error (show err)
           Right x -> (fromJust . Yaml.parseMaybe Yaml.parseJSON  $ x)
         dirId = takeFileName . takeDirectory $ f
         cfg  = Yaml.Object (M.insert (T.pack "id") (Yaml.String (T.pack dirId)) yml)
     return cfg
