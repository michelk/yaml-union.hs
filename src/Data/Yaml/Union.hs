module Data.Yaml.Union
  ( decodeBytestrings
  , decodeBytestringsEither
  , decodeFiles
  , decodeFilesEither
  ) where

import           Data.ByteString (ByteString)
import           Data.Foldable
import qualified Data.HashMap.Strict as M
import           Data.Yaml hiding (decodeFile, decodeFileEither)
import           Data.Yaml.Include
import           Data.Maybe (mapMaybe, fromJust )
import           Data.Vector (Vector)
import qualified Data.Vector as Vec

-- | Decode multiple YAML strings and override fields recursively
decodeBytestrings
  :: FromJSON a
  => [ByteString] -> Maybe a
decodeBytestrings = parseMaybe parseJSON . Object . unions . mapMaybe decode

-- | Decode multiple YAML strings and override fields recursively
decodeBytestringsEither
  :: FromJSON a
  => [ByteString] -> Either String a
decodeBytestringsEither =
  parseEither parseJSON . Object . unions . mapMaybe decode

-- | Decode multiple YAML-files and override fields recursively
decodeFiles :: FromJSON a => [FilePath] -> IO (Maybe a)
decodeFiles fs = do
  s <-
    mapM
      (\f -> do
         s <- decodeFile f
         return $ fromJust s)
      fs
  return $ parseMaybe parseJSON . Object . unions $ s

-- | Decode multiple YAML-files and override fields recursively
decodeFilesEither :: FromJSON a => [FilePath] -> IO (Either String a)
decodeFilesEither fs = do
  s <-
    mapM
      (\f -> do
         s <- decodeFileEither f
         case s of
           Left err -> error (show err)
           Right x -> return x)
      fs
  return $ parseEither parseJSON . Object . unions $ s


unions :: [Object] -> Object
unions = foldl' union M.empty

union ::  Object ->  Object -> Object
union = M.unionWith dispatch

dispatch :: Value -> Value -> Value
dispatch (Object v1) (Object v2) = Object (v1 `union` v2)
dispatch (Array v1) (Array v2) = Array $ vecUnion v1 v2
dispatch _ x   = x

vecUnion
  :: Eq a
  => Vector a -> Vector a -> Vector a
vecUnion = vecUnionBy (==)

vecUnionBy :: (a -> a -> Bool) -> Vector a -> Vector a -> Vector a
vecUnionBy eq xs ys =
  (Vec.++) xs (foldl (flip (vecDeleteBy eq)) (vecNubBy eq ys) xs)

vecDeleteBy :: (a -> a -> Bool) -> a -> Vector a -> Vector a
vecDeleteBy eq x ys
  | Vec.length ys == 0 = ys
  | otherwise =
    if x `eq` Vec.head ys
      then Vec.tail ys
      else Vec.head ys `Vec.cons` vecDeleteBy eq x (Vec.tail ys)

vecNubBy :: (a -> a -> Bool) -> Vector a -> Vector a
vecNubBy eq vec
  | Vec.length vec == 0 = vec
  | otherwise =
    Vec.head vec `Vec.cons`
    vecNubBy eq (Vec.filter (not . eq (Vec.head vec)) (Vec.tail vec))
