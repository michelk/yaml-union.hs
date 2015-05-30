module Data.Yaml.Override (decodeFiles) where
import qualified Data.HashMap.Strict as M
import           Data.Maybe (catMaybes)
import           Data.Yaml

-- | Decode multiple YAML-files and override recurisvley field
decodeFiles :: FromJSON a => [FilePath] -> IO (Maybe a)
decodeFiles fs =  fmap (parseMaybe parseJSON . Object) (readFiles fs)

readFiles :: [FilePath] -> IO Object
readFiles fs =
  do cfgs <- mapM decodeFile fs
     return . unions . catMaybes $ cfgs

unions :: [Object] -> Object
unions =
  -- M.unions  . reverse
  foldl1 union

union ::  Object ->  Object -> Object
union = M.unionWith dispatch

dispatch :: Value -> Value -> Value
dispatch (Object v1) (Object v2) = Object (v1 `union` v2)
dispatch _ x  = x

