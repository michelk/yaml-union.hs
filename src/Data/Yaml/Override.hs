module Data.Yaml.Override (decodeFiles) where
import qualified Data.HashMap.Strict as M
import           Data.Maybe (catMaybes, fromJust)
import           Data.Yaml

decodeFiles :: FromJSON a => [FilePath] -> IO (Maybe a)
decodeFiles fs =  fmap (parseMaybe parseJSON . Object) (readFiles fs)

readFiles :: [FilePath] -> IO Object
readFiles fs =
  do cfgs <- mapM decodeFile fs
     return $ unions $ catMaybes cfgs

unions :: [Object] -> Object
unions = foldl1 union

union ::  Object ->  Object -> Object
union m1 m2 = foldr update m1 ks2
  where
    ks2 = M.keys m2
    update x = M.adjust (\_ -> fromJust $ M.lookup x m2) x
