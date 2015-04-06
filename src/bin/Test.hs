{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import System.IO (Handle, hClose, openTempFile, hFlush, hSeek, SeekMode(AbsoluteSeek))
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO.Error (catchIOError)
import Control.Exception (finally)

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
    , hUnitTestToTests $ TestLabel "optionalIntersectionWith Tests" (mapTestCase optionalIntersectionWithData)
    , hUnitTestToTests $ TestLabel "findMovedFiles Tests" (mapTestCase findMovedFilesData)
    , hUnitTestToTests $ TestLabel "foldlMap Tests" (mapTestCase foldlMapData)
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
    [ ("Empty Map",         runEmitHashMap "",              Map.empty)
    , ("One Map",           runEmitHashMap "a b",           Map.singleton "a" (Set.singleton "b"))
    , ("One Map Newline",   runEmitHashMap "a b\n",         Map.singleton "a" (Set.singleton "b"))
    , ("Two Map",           runEmitHashMap "a b\nc d",      Map.fromList [("a", Set.singleton "b"), ("c", Set.singleton "d")])
    , ("Two Map Overlap",   runEmitHashMap "a b\na c\n",    Map.singleton "a" (Set.fromAscList ["b", "c"]))
    , ("Two Map Overwrite", runEmitHashMap "a b\na b\n",    Map.singleton "a" (Set.singleton "b"))
    ]
  where
    runEmitHashMap input = withTempFile "emitHashMap" (\_ h -> flushInput h input >> emitHashMap h)

-- Output the bytestring to a file then reset to start of file for reading back in.
flushInput :: Handle -> ByteString -> IO ()
flushInput h x = do
    BS.hPut h x
    hFlush h
    hSeek h AbsoluteSeek 0

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func = do
   tempdir <- catchIOError (getTemporaryDirectory) (\_ -> return ".")
   (tempfile, temph) <- openTempFile tempdir pattern

   finally (func tempfile temph)
           (do hClose temph
               removeFile tempfile)

-- TODO: same issue as intersectionwith - should rename things from src dst
-- to inital and final state to be clearer as of what is going on.
findMovedFilesData =
    [ ("Empty Sets",
        findMovedFiles Set.empty Set.empty,
        Nothing)

    -- TODO: maybe wrong?
    , ("Remove Sets",
        findMovedFiles (Set.singleton "a") Set.empty,
        Just $ Removed $ Set.singleton "a")

    -- TODO: maybe wrong?
    , ("Add Sets",
        findMovedFiles Set.empty (Set.singleton "a"),
        Just $ Added $ Set.singleton "a")

    -- Moved - most complicated bits
    , ("Same Moved",
        findMovedFiles (Set.singleton "a") (Set.singleton "a"),
        Nothing)

    , ("One Moved",
        findMovedFiles (Set.singleton "a") (Set.singleton "b"),
        Just $ Moved "b" (Set.singleton "a") (Set.singleton "b"))

    -- TODO: should just be a removal "b" the "a" is already there
    , ("One Moved - Two Destination - One Same",
        findMovedFiles (Set.singleton "a") (Set.fromAscList ["a", "b"]),
        Just $ Moved "a" (Set.singleton "a") (Set.singleton "b"))

    , ("One Moved - Two Destination - Nothing Same",
        findMovedFiles (Set.singleton "a") (Set.fromAscList ["b", "c"]),
        Just $ Moved "b" (Set.singleton "a") (Set.fromAscList ["b", "c"]))

    -- TODO: should only be one destination ("b") for copying to, no need to copy to "a"
    , ("One Moved - Two Source - One Same",
        findMovedFiles (Set.fromAscList ["a", "b"]) (Set.singleton "a"),
        Just $ Moved "a" (Set.fromAscList ["a", "b"]) (Set.empty))

    , ("One Moved - Two Source - Nothing Same",
        findMovedFiles (Set.fromAscList ["a", "b"]) (Set.singleton "c"),
        Just $ Moved "c" (Set.fromAscList ["a", "b"]) (Set.singleton "c"))

    , ("Two Moved - Two Source - Two Same",
        findMovedFiles (Set.fromAscList ["a", "b"]) (Set.fromAscList ["a", "b"]),
        Nothing)

    -- TODO: "a" already exists, so shouldn't copy over "a"
    , ("Two Moved - Two Source - One Same",
        findMovedFiles (Set.fromAscList ["a", "b"]) (Set.fromAscList ["a", "c"]),
        Just $ Moved "a" (Set.fromAscList ["a", "b"]) (Set.singleton "c"))

    , ("Two Moved - Two Source - Nothing Same",
        findMovedFiles (Set.fromAscList ["a", "b"]) (Set.fromAscList ["c", "d"]),
        Just $ Moved "c" (Set.fromAscList ["a", "b"]) (Set.fromAscList ["c", "d"]))
    ]

-- This is a little confusing we may want to rename our states a bit
-- instead of src dst and reorder it to "old state" "new state" or so.
optionalIntersectionWithData =
    [ ("Empty Map",
        optionalIntersectionWith doNothing Map.empty Map.empty,
        Map.empty)

    -- TODO: somewhat suspecting that the order on these are backward (src/dst)
    -- TODO: maybe wrong?
    , ("Added Map",
        optionalIntersectionWith doNothing Map.empty (Map.singleton "a" (Set.singleton "b")),
        Map.singleton "a" (Added $ Set.singleton "b"))

    -- TODO: maybe wrong?
    , ("Removed Map",
        optionalIntersectionWith doNothing (Map.singleton "a" (Set.singleton "b")) Map.empty,
        Map.singleton "a" (Removed $ Set.singleton "b"))

    -- With doNothing nothing should happen
    , ("Dummy Moved Map",
        optionalIntersectionWith doNothing (Map.singleton "a" (Set.singleton "b")) (Map.singleton "a" (Set.singleton "c")),
        Map.empty)

    -- Testing with actual findMovedFiles
    ]
  where
    doNothing _ _ = Nothing


--foldlMap :: Action -> ByteString
foldlMapData =
    [ ("Empty - Removed",   foldlMap (Removed (Set.empty)),     "")
    ]

--data Action = Removed (Set ByteString) -- Remove
--            | Added (Set ByteString) -- Local file to Upload
--            | Moved ByteString (Set ByteString) (Set ByteString) -- Src to copy from, added files (to copy to), removed files (to remove afterward)
--            deriving (Show, Eq)
