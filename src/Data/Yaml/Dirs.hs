{-# LANGUAGE OverloadedStrings #-}
module Data.Yaml.Dirs (unionDirs, decodeFiles) where

import qualified Data.Yaml as Yaml
import           Data.Maybe (fromJust)
import           System.FilePath
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.List (sort)
import Control.Monad
import System.Directory
import System.FilePath
import System.Posix.Files

-- | Union a list directories with a common filename withem them
unionDirs :: FilePath           -- ^ Filepath to union on
  -> [FilePath]                 -- ^ directories to traverse
  -> IO Yaml.Value
unionDirs file dirs = do
  dirsCfg <- mapM (readDir file) dirs
  return (Yaml.Array . V.fromList $ dirsCfg)

readDir :: FilePath -> FilePath -> IO Yaml.Value
readDir file d = do
       let path = d </> file
       hasF <- doesFileExist path
       r <-
         if hasF
           then do
             yml <- Yaml.decodeFileEither path
             let yml' =
                   case yml of
                     Left err -> error (show err)
                     Right x -> fromJust . Yaml.parseMaybe Yaml.parseJSON $ x
                 dirId = takeFileName d
                 cfg = M.insert (T.pack "id") (Yaml.String (T.pack dirId)) yml'
             return cfg
           else return M.empty
       dirs <- getDirectoryDirs d
       dd <-
         if length dirs == 0
           then return r
           else do
              sub <- unionDirs file  [d </> subdir | subdir <- dirs]
              return (M.insert (T.pack "subdirs") sub r)
       return (Yaml.Object dd)

getDirectoryDirs :: FilePath -> IO [FilePath]
getDirectoryDirs d =
  do fs <- getDirectoryContents d
     let fs' = filter ( `notElem` [".",".." ]) fs
     filterM (\f -> doesDirectoryExist (d </> f))fs'

-- | Decode directories either reverse sorted or not 
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
