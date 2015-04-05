{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import System.IO (Handle)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

import Pipes (Producer, (>->))
import qualified Pipes as P
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB

import Pipes.ByteString (ByteString)

import Data.Attoparsec.ByteString (parseOnly)

import Control.Monad
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import FileHash (emitHashMap, updateMap, pipeHashLine, hashLine)
import HashDiff (foldlMap, findMovedFiles, optionalIntersectionWith, Action(..))


main :: IO ()
main = defaultMain $ concat
    [ hUnitTestToTests $ TestLabel "hashLine Tests" (mapTestCase hashLineData)
    , hUnitTestToTests $ TestLabel "pipeHashLine Tests" (mapTestCaseM pipeHashLineData)
    , hUnitTestToTests $ TestLabel "updateMap Tests" (mapTestCase updateMapData)
    , hUnitTestToTests $ TestLabel "emitHashMap Tests" (mapTestCaseM emitHashMapData)
    ]

mapTestCase xs = TestList $ (map (\(l, i, o) -> (TestLabel l $ TestCase (i @?= o))) xs)
mapTestCaseM xs = TestList $ (map (\(l, i, o) -> (TestLabel l $ TestCase (i >>= (@?=) o))) xs)

hashLineData =
    [ ("Empty",                 parseOnly hashLine "",          Left "not enough input")
    , ("Single Line",           parseOnly hashLine "a b",       Right ("a", "b"))
    , ("Single Line Newline",   parseOnly hashLine "a b\n",     Right ("a", "b"))
    , ("Two Line",              parseOnly hashLine "a b\nc d",  Right ("a", "b"))
    , ("Whitespace Filename",   parseOnly hashLine "a b c\n",   Right ("a", "b c"))
    ]

pipeHashLineData =
    [ ("Empty",                 runHashLine "",         [])
    , ("Single Line",           runHashLine "a b",      [("a", "b")])
    , ("Single Line Newline",   runHashLine "a b\n",    [("a", "b")])
    , ("Two Line",              runHashLine "a b\nc d", [("a", "b"), ("c", "d")])
    , ("Whitespace Filename",   runHashLine "a b c",    [("a", "b c")])
    ]
  where
    runHashLine input = P.runEffect $ P.toListM (pipeHashLine $ PB.fromLazy input)

updateMapData =
    [ ("Empty Map",     updateMap Map.empty ("a", "b"),                                 Map.singleton "a" (Set.singleton "b"))
    , ("One Map",       updateMap (Map.singleton "a" (Set.singleton "a")) ("a", "b"),   Map.singleton "a" (Set.fromAscList ["a", "b"]))
    , ("Two Map",       updateMap (Map.singleton "a" (Set.singleton "a")) ("b", "b"),   Map.fromList [("a", Set.singleton "a"), ("b", Set.singleton "b")])
    , ("Overwrite Map", updateMap (Map.singleton "a" (Set.singleton "a")) ("a", "a"),   Map.singleton "a" (Set.singleton "a"))
    ]

emitHashMapData =
    [ ("", return "", "")
    ]
--emitHashMap :: Handle -> IO (Map ByteString (Set ByteString))
