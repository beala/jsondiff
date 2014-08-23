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
         t1 <- Atto.parseOnly json (fst args) 
         t2 <- Atto.parseOnly json (snd args)
         return $ diffJson t1 t2 [] []

parseArgs :: [String] -> (B.ByteString, B.ByteString)
parseArgs rawArgs = (B.pack (rawArgs !! 0), B.pack (rawArgs !! 1))

type Path = [String] 
data Diff = Diff Path (Maybe Value) (Maybe Value) deriving Show

diffJson :: Value -> Value -> Path -> [Diff] -> [Diff]
diffJson (Object o1) (Object o2) p d = undefined
diffJson (Array a1) (Array a2) p d =
    let matchedDiffs = (Vec.foldr diffElems d elemPairs) in 
    diffUnmatchedArrayElems a1 a2 ("array" : p) matchedDiffs
        where elemPairs = Vec.zip a1 a2
              diffElems (aa1, aa2) dd = diffJson aa1 aa2 ("array" : p) dd
diffJson s1@(String ss1) s2@(String ss2) p d = diffValues (s1, ss1) (s2, ss2) p d
diffJson n1@(Number nn1) n2@(Number nn2) p d = diffValues (n1, nn1) (n2, nn2) p d
diffJson b1@(Bool bb1) b2@(Bool bb2) p d     = diffValues (b1, bb1) (b2, bb2) p d
diffJson Null Null p d                       = d
diffJson j1 j2 p d                           = (Diff p (Just j1) (Just j2)) : d

diffUnmatchedArrayElems :: Vec.Vector Value -> Vec.Vector Value -> Path -> [Diff] -> [Diff]
diffUnmatchedArrayElems a1 a2 p d
    | Vec.length a1 > Vec.length a2 =
        let unmatched = Vec.drop (Vec.length a2) a1 in
        Vec.toList $ Vec.map (\a -> (Diff p (Just a) Nothing)) unmatched
    | Vec.length a1 < Vec.length a2 =
        let unmatched = Vec.drop (Vec.length a1) a2 in
        Vec.toList $ Vec.map (\a -> (Diff p Nothing (Just a))) unmatched
    | otherwise = []

diffValues :: (Eq a) => (Value, a) -> (Value, a) -> Path -> [Diff] -> [Diff]
diffValues (v1, vv1) (v2, vv2) p d
    | vv1 /= vv2  = (Diff p (Just v1) (Just v2)) : d
    | otherwise = d
