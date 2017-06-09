{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnionSpec where

import           Data.Char
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import           Data.Yaml
import           Data.Yaml.Union
import           Test.Hspec
import           Test.QuickCheck
-- import           Test.QuickCheck.Instances ()

-- Null gets omitted in encoding so we are not including it here
instance Arbitrary Value where
  arbitrary =
    oneof
      [ Object <$>
        resize
          5
          arbitrary
      , Array <$> resize 5 arbitrary
      , String <$> arbitrary
      , Number <$> arbitrary
      , Bool <$> arbitrary
      ]

-- hash maps with keys that are not empty and contain ASCII characters only
instance (Arbitrary v) =>
         Arbitrary (HashMap Text v) where
  arbitrary =
    HashMap.fromList <$>
    (arbitrary `suchThat`
     (\t ->
        all ((> 0) . Text.length . fst) t && all (Text.all isAsciiLower . fst) t))

instance Arbitrary a => Arbitrary (Vector a) where
  arbitrary = Vec.fromList <$> arbitrary

instance Arbitrary Text where
  arbitrary = Text.pack <$> arbitrary

instance Arbitrary Scientific where
    arbitrary = do
        c <- arbitrary
        e <- arbitrary
        return $ scientific c e

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Union" $ do
    it "with itself == id" $
      property $ \(x :: Object) ->
        decodeBytestrings [encode x, encode x] `shouldBe` Just x
    it "preserves keys" $
      property $ \(x :: Object, y :: Object) ->
        collectKeys <$>
        (decodeBytestrings [encode x, encode y] :: Maybe Value) `shouldBe`
        Just (collectKeys (Object x) ++ collectKeys (Object y))
    it "decompose" $
      property $ \(x :: Object) ->
        (decodeBytestrings $ map (encode . Object) $ decompose x :: Maybe Value) `shouldBe`
        Just (Object x)

collectKeys :: Value -> [Text]
collectKeys (Object o) =
  HashMap.keys o ++ concatMap collectKeys (HashMap.elems o)
collectKeys _ = []

decompose :: HashMap Text v -> [HashMap Text v]
decompose m
  | HashMap.size m == 0 = []
  | otherwise =
    let t = HashMap.fromList $ tail $ HashMap.toList m
    in m : decompose t
