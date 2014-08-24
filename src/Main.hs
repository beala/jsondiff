{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson.Encode
import           Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.HashMap.Strict as HMap
import           Data.Hashable
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as Vec
import           System.Environment

main :: IO ()
main = do
  rawArgs <- getArgs
  let args = parseArgs rawArgs
  let diffs = do
         t1 <- Atto.parseOnly value (fst args) 
         t2 <- Atto.parseOnly value (snd args)
         return $ diff (Just t1) (Just t2)
  putStr $ prettyPrintDiffs diffs

parseArgs :: [String] -> (B.ByteString, B.ByteString)
parseArgs rawArgs = (B.pack (rawArgs !! 0), B.pack (rawArgs !! 1))

data PathElem = Field String
              | ArrayIndex Int
              | Root

instance Show PathElem where
    show (Field s) = s
    show (ArrayIndex i) = "array[" ++ (show i) ++ "]"
    show Root = "root"

type Path = [PathElem] 
data Diff = Diff Path (Maybe Value) (Maybe Value) deriving Show

diff :: Maybe Value -> Maybe Value -> [Diff]
diff v1 v2 = fmap reversePath (diffMaybeJson v1 v2 [Root] [])
             where reversePath (Diff p n1 n2) = (Diff (reverse p) n1 n2)

diffMaybeJson :: Maybe Value -> Maybe Value -> Path -> [Diff] -> [Diff]
diffMaybeJson (Just v1) (Just v2) p d = diffJson v1 v2 p d
diffMaybeJson mV1 mV2 p d = (Diff p mV1 mV2) : d

diffJson :: Value -> Value -> Path -> [Diff] -> [Diff]
diffJson (Object o1) (Object o2) p d =
    foldr diffElems d fieldValueList
        where fieldValueList = Map.toList (mergeObjectMap (hashMapToMap o1) (hashMapToMap o2))
              diffElems (f, (v1, v2)) dd = diffMaybeJson v1 v2 (Field (T.unpack f) : p) dd
diffJson (Array a1) (Array a2) p d =
    Vec.foldr diffElems d elemPairs
        where elemPairs = izipAll (Vec.map Just a1) (Vec.map Just a2) Nothing Nothing
              diffElems (i, aa1, aa2) dd = diffMaybeJson aa1 aa2 (ArrayIndex i : p) dd
diffJson s1@(String ss1) s2@(String ss2) p d = diffLeaf (s1, ss1) (s2, ss2) p d
diffJson n1@(Number nn1) n2@(Number nn2) p d = diffLeaf (n1, nn1) (n2, nn2) p d
diffJson b1@(Bool bb1) b2@(Bool bb2) p d     = diffLeaf (b1, bb1) (b2, bb2) p d
diffJson Null Null _ d                       = d
diffJson j1 j2 p d                           = (Diff p (Just j1) (Just j2)) : d

diffLeaf :: (Eq a) => (Value, a) -> (Value, a) -> Path -> [Diff] -> [Diff]
diffLeaf (v1, vv1) (v2, vv2) p d
    | vv1 /= vv2  = (Diff p (Just v1) (Just v2)) : d
    | otherwise = d

izipAll :: Vec.Vector a -> Vec.Vector b -> a -> b -> Vec.Vector (Int, a, b)
izipAll v1 v2 pad1 pad2 = Vec.izipWith (\i vv1 vv2 -> (i, vv1, vv2)) paddedV1 paddedV2
    where padLen = max (Vec.length v1) (Vec.length v2)
          paddedV1 = padTo v1 pad1 padLen
          paddedV2 = padTo v2 pad2 padLen

padTo :: Vec.Vector a -> a -> Int -> Vec.Vector a
padTo v p l
    | (Vec.length v) < l =
        let paddingLen = l - Vec.length v in
        v Vec.++ (Vec.replicate paddingLen p)
    | otherwise = v

hashMapToMap :: (Ord k, Eq k, Hashable k) => HMap.HashMap k v -> Map.Map k v
hashMapToMap = Map.fromList . HMap.toList

mergeObjectMap :: Map.Map T.Text Value -> Map.Map T.Text Value -> Map.Map T.Text (Maybe Value, Maybe Value)
mergeObjectMap = Map.mergeWithKey combine only1 only2
    where combine _ x1 x2 = Just ((Just x1), (Just x2))
          only1 = Map.map (\v -> (Just v, Nothing))
          only2 = Map.map (\v -> (Nothing, Just v))

prettyPrintDiffs :: Either String [Diff] -> String
prettyPrintDiffs (Left parseError) = "Error parsing json: " ++ parseError ++ "\n"
prettyPrintDiffs (Right ds)     = unlines $ fmap prettyPrintDiff ds

prettyPrintDiff :: Diff -> String
prettyPrintDiff (Diff p v1 v2) = (prettyPrintPath p)
                                 ++ "\n\t"
                                 ++ (prettyPrintMaybeValue v1)
                                 ++ "\n\t"
                                 ++ (prettyPrintMaybeValue v2)
                                 ++ ""

prettyPrintPath :: Path -> String
prettyPrintPath = L.intercalate "." . fmap show

prettyPrintMaybeValue :: Maybe Value -> String
prettyPrintMaybeValue (Just v) = prettyPrintValue v
prettyPrintMaybeValue Nothing = "Missing"

prettyPrintValue :: Value -> String
prettyPrintValue = LB.unpack . encode
