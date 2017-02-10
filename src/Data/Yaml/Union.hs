module Data.Yaml.Union (decodeFiles, decodeFilesEither) where
import qualified Data.HashMap.Strict as M
import           Data.Maybe (catMaybes)
import           Data.Yaml hiding (decodeFile)
import           Data.Yaml.Include (decodeFile)

-- | Decode multiple YAML-files and override recurisvley field
decodeFiles :: FromJSON a => [FilePath] -> IO (Maybe a)
decodeFiles fs =  fmap (parseMaybe parseJSON . Object) (readFiles fs)

-- | Decode multiple YAML-files and override recurisvley field
decodeFilesEither :: FromJSON a => [FilePath] -> IO (Either String a)
decodeFilesEither fs = fmap (parseEither parseJSON . Object) (readFiles fs)

readFiles :: [FilePath] -> IO Object
readFiles fs =
  do cfgs <- mapM decodeFile fs
     return . unions . catMaybes $ cfgs

unions :: [Object] -> Object
unions = foldl1 union

union ::  Object ->  Object -> Object
union = M.unionWith dispatch

dispatch :: Value -> Value -> Value
dispatch (Object v1) (Object v2) = Object (v1 `union` v2)
--dispatch (Array v1) (Array v2) = undefined
dispatch _ x   = x
