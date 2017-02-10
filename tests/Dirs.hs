import qualified Data.Yaml as Y
import Data.Yaml.Pretty
import Data.Yaml.Dirs
import qualified Data.ByteString.Char8 as B

main:: IO ()
main =
  do vs <- unionDirs "de.yaml" "./example"
     let s = encodePretty defConfig vs
     B.putStrLn s
