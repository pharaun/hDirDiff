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
            (  metavar "INITIAL"
            <> help "The initial state checksum file (destination files)"
            )
        <*> strArgument
            (  metavar "FINAL"
            <> help "The final state checksum file (source files)"
            )

main = do
    (initialFile, finalFile) <- getArgs

    -- Load up the hashes from the initial/final files.
    (initialMap, finalMap) <- concurrently (withFile initialFile ReadMode emitHashMap) (withFile finalFile ReadMode emitHashMap)

    -- 1. Identify new files
    -- 2. Identify removed files
    -- 3. Identify actions needed (Moved, Removed, Added)
    let changes = optionalIntersectionWith findMovedFiles initialMap finalMap

    let new = Map.filter (\v -> case v of
                                        Removed _   -> False
                                        Added _     -> True
                                        Moved _ _ _ -> False) changes

    let removed = Map.filter (\v -> case v of
                                        Removed _   -> True
                                        Added _     -> False
                                        Moved _ _ _ -> False) changes

    let moved = Map.filter (\v -> case v of
                                        Removed _   -> False
                                        Added _     -> False
                                        Moved _ _ _ -> True) changes

    -- Quick verification (we do simple new/removed file)
    let new'     = Map.difference finalMap initialMap
    let removed' = Map.difference initialMap finalMap

    -- Emit some information about the current state
    putStrLn "Initial Map Size"
    putStrLn $ "Initial : " ++ (show $ Map.size initialMap)
    putStrLn $ "Final   : " ++ (show $ Map.size finalMap)

    putStrLn "Changes Map Size"
    putStrLn $ "Added   : " ++ (show $ Map.size new)
    putStrLn $ "Removed : " ++ (show $ Map.size removed)
    putStrLn $ "Moved   : " ++ (show $ Map.size moved)

    putStrLn "Verification Map Size"
    putStrLn $ "Added   : " ++ (show $ Map.size new')
    putStrLn $ "Removed : " ++ (show $ Map.size removed')

    putStrLn "Are they the same?"
    putStrLn $ "Added   : " ++ show (new == (Map.map Added new'))
    putStrLn $ "Removed : " ++ show (removed == (Map.map Removed removed'))

    -- Emit an rough output of the action needed to bring initial state up
    -- to spec (final state)
    let output = Map.foldl (\str v -> BS.concat [str, foldlMap v]) (C8.pack "") changes
    C8.hPutStr stdout output
