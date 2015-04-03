import Control.Concurrent.Async
import Control.Applicative
import System.IO (withFile, IOMode(ReadMode), stdout)

import qualified Data.Map.Strict as Map


import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8



import Options.Applicative hiding (Parser, str)


import HashDiff (foldlMap, findMovedFiles, optionalIntersectionWith, Action(..))
import FileHash (emitHashMap)


-- CLI options
getArgs :: IO (FilePath, FilePath)
getArgs = execParser opts
  where
    opts = info (helper <*> targets)
        (  fullDesc
        <> progDesc "Run the checksum differ for emitting aws s3 commands"
        <> header "hDirDiff - A checksum differ" )
    targets = (,)
        <$> strArgument
            (  metavar "SOURCE"
            <> help "The source checksum file"
            )
        <*> strArgument
            (  metavar "DESTINATION"
            <> help "The destination checksum file"
            )


-- TODO: output matches within of ~500 difference out of 55k action between
-- dominikh and my implementation, this means there's either some minor
-- variation in what we pick to delete/copy/up or one of our impl isn't
-- quite doing something right. This needs to have lots of tests.
--
-- Tests:
--  1. One upload
--  2. One removal
--  3. One move (A -> B)
--  4. One to many Copy (A -> B, A -> C)
--  5. Overlapping upload/removal/move

main = do
    (src, dst) <- getArgs

    -- Handle source hash
    (srcMap, dstMap) <- concurrently (withFile src ReadMode emitHashMap) (withFile dst ReadMode emitHashMap)

    -- 1. Identify files that has been removed from the destination
    --      (check hashes that are not present)
    --      (Probably want to do remove last, but don't want to remove
    --          overwritten files)
    -- TODO: this can be done with just the findMovedFile and mergeWithKey
    let removed = Map.difference srcMap dstMap

    -- 2. Identify files that has been added to the destination
    --      (check hashes that are new)
    --      (If multiple new file with no destination, should upload one,
    --          then copy the rest)
    -- TODO: this can be done with just the findMovedFile and mergeWithKey
    let new = Map.difference dstMap srcMap

    -- 3. Identify moved files
    let moved = optionalIntersectionWith findMovedFiles srcMap dstMap

    -- 4. Emit changed list (added/removed/moved)
    putStrLn $ "Src     - Initial: " ++ (show $ Map.size srcMap)
    putStrLn $ "Dst     - Initial: " ++ (show $ Map.size dstMap)

    putStrLn $ "Removed - Union: " ++ (show $ Map.size removed)
    putStrLn $ "Added   - Union: " ++ (show $ Map.size new)

    -- Filter the moved into removed/added/moved
    let removed' = Map.filter (\v -> case v of
                                        Removed _   -> True
                                        Added _     -> False
                                        Moved _ _ _ -> False) moved

    let new' = Map.filter (\v -> case v of
                                        Removed _   -> False
                                        Added _     -> True
                                        Moved _ _ _ -> False) moved

    let moved' = Map.filter (\v -> case v of
                                        Removed _   -> False
                                        Added _     -> False
                                        Moved _ _ _ -> True) moved

    putStrLn $ "Removed - Merge: " ++ (show $ Map.size removed')
    putStrLn $ "Added   - Merge: " ++ (show $ Map.size new')
    putStrLn $ "Moved   - Merge: " ++ (show $ Map.size moved')

    putStrLn $ "Removed - Merge - Union: " ++ show (Map.map (\(Removed x) -> x) removed' == removed)
    putStrLn $ "Added   - Merge - Union: " ++ show (Map.map (\(Added x) -> x) new' == new)

    -- Compare the two map and create a "final map"
    --
    -- For comparing with test.awk emit the test.awk output format
    let output = Map.foldl (\str v -> BS.concat [str, foldlMap v]) (C8.pack "") moved
    C8.hPutStr stdout output
