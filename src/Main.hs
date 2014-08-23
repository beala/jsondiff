{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString.Char8 as B
import           System.Environment
import qualified Data.Vector as Vec

main :: IO ()
main = do
  rawArgs <- getArgs
  let args = parseArgs rawArgs
  print $ do
         t1 <- Atto.parseOnly value (fst args) 
         t2 <- Atto.parseOnly value (snd args)
         return $ diffMaybeJson (Just t1) (Just t2) [] []

parseArgs :: [String] -> (B.ByteString, B.ByteString)
parseArgs rawArgs = (B.pack (rawArgs !! 0), B.pack (rawArgs !! 1))

type Path = [String] 
data Diff = Diff Path (Maybe Value) (Maybe Value) deriving Show

diffMaybeJson :: Maybe Value -> Maybe Value -> Path -> [Diff] -> [Diff]
diffMaybeJson (Just v1) (Just v2) p d = diffJson v1 v2 p d
diffMaybeJson mV1 mV2 p d = (Diff p mV1 mV2) : d

diffJson :: Value -> Value -> Path -> [Diff] -> [Diff]
diffJson (Object o1) (Object o2) p d = undefined
diffJson (Array a1) (Array a2) p d =
    Vec.foldr diffElems d elemPairs
        where elemPairs = zipAll (Vec.map Just a1) (Vec.map Just a2) Nothing Nothing
              diffElems (aa1, aa2) dd = diffMaybeJson aa1 aa2 ("array" : p) dd
diffJson s1@(String ss1) s2@(String ss2) p d = diffValues (s1, ss1) (s2, ss2) p d
diffJson n1@(Number nn1) n2@(Number nn2) p d = diffValues (n1, nn1) (n2, nn2) p d
diffJson b1@(Bool bb1) b2@(Bool bb2) p d     = diffValues (b1, bb1) (b2, bb2) p d
diffJson Null Null _ d                       = d
diffJson j1 j2 p d                           = (Diff p (Just j1) (Just j2)) : d

diffValues :: (Eq a) => (Value, a) -> (Value, a) -> Path -> [Diff] -> [Diff]
diffValues (v1, vv1) (v2, vv2) p d
    | vv1 /= vv2  = (Diff p (Just v1) (Just v2)) : d
    | otherwise = d

--printDiff :: Diff -> IO ()
--printDiff (Diff p v1 v2) = putStrLn (reverse p) ++ " " ++ v1 ++ " " ++ v2

zipAll :: Vec.Vector a -> Vec.Vector b -> a -> b -> Vec.Vector (a, b)
zipAll v1 v2 pad1 pad2 = Vec.zip paddedV1 paddedV2
    where padLen = max (Vec.length v1) (Vec.length v2)
          paddedV1 = padTo v1 pad1 padLen
          paddedV2 = padTo v2 pad2 padLen

padTo :: Vec.Vector a -> a -> Int -> Vec.Vector a
padTo v p l
    | (Vec.length v) < l =
        let paddingLen = l - Vec.length v in
        v Vec.++ (Vec.replicate paddingLen p)
    | otherwise = v
