import Control.Concurrent.Async
import Control.Applicative
import Control.Monad.Trans.State.Strict
import Data.Char
import System.IO (withFile, IOMode(ReadMode), Handle, stdout)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

import Pipes (Producer)
import qualified Pipes as P
import qualified Pipes.Prelude as P
import qualified Pipes.Attoparsec as PA
import Pipes.ByteString (Word8, ByteString)
import qualified Pipes.ByteString as PB

import Data.Attoparsec.ByteString (Parser, skip, takeWhile1)

import Options.Applicative hiding (Parser, str)

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

foldlMap :: Action -> ByteString
foldlMap (Removed x)    = Set.foldl (\str v -> BS.concat [str, C8.pack "rm r/", v, C8.pack "\n"]) (C8.pack "") x
foldlMap (Added x)      = do
    let up = Set.elemAt 0 x
    let cp = Set.delete up x
    BS.concat
        [ C8.pack "up s/", up, C8.pack "\n"
        , Set.foldl (\str v -> BS.concat [str, C8.pack "cp r/", up, C8.pack " r/", v, C8.pack "\n"]) (C8.pack "") cp
        ]
foldlMap (Moved s a r)  = BS.concat
    [ Set.foldl (\str v -> BS.concat [str, C8.pack "cp r/", s, C8.pack " r/", v, C8.pack "\n"]) (C8.pack "") a
    , Set.foldl (\str v -> BS.concat [str, C8.pack "rm r/", v, C8.pack "\n"]) (C8.pack "") r
    ]

-- | Moved ByteString (Set ByteString) (Set ByteString) -- Src to copy from, added files (to copy to), removed files (to remove afterward)

--      a. Compare file list, if same then move on
--      b. If src is not empty and dst is empty, we removed files copies
--      c. If dst is not empty and src is empty, we added file copies
--      d. If dst and source is not empty, we have some sort of moves
--          (Since all source and all dest is the same, can probably make
--              it simpler and just do multiple copies from one
--              source/dest to the rest)
findMovedFiles :: Set ByteString -> Set ByteString -> Maybe Action
findMovedFiles src dst
    -- Exactly the same, no change
    | src == dst                            = Nothing
    -- Shouldn't happen?
    | (not $ Set.null src) && Set.null dst  = Just $ Removed src
    -- Shouldn't happen?
    | Set.null src && (not $ Set.null dst)  = Just $ Added dst
    -- Both src & dest is not empty let's figure out what kind of move it is
    --
    -- We can just grab the first file of the destination and use that
    -- as the source of the "copy-from" operation to fullfill the
    -- added files list then apply the removed list.
    | otherwise                             = Just $ Moved (Set.elemAt 0 dst) (Set.difference src (Set.delete (Set.elemAt 0 dst) dst)) (Set.difference dst src)

data Action = Removed (Set ByteString) -- Remove
            | Added (Set ByteString) -- Local file to Upload
            | Moved ByteString (Set ByteString) (Set ByteString) -- Src to copy from, added files (to copy to), removed files (to remove afterward)
            deriving Show


-- | Extended version of 'intersectionWith' that enables dropping of
-- elements if the combining function returns a 'Nothing'
-- TODO: find a way to make the types more generic
--optionalIntersectionWith :: Ord k => (a -> b -> Maybe c) -> Map k a -> Map k b -> Map k c
optionalIntersectionWith :: (Set ByteString -> Set ByteString -> Maybe Action) -> Map ByteString (Set ByteString) -> Map ByteString (Set ByteString) -> Map ByteString Action
optionalIntersectionWith f t1 t2 = Map.mergeWithKey (\_ x1 x2 -> f x1 x2) (Map.map Removed) (Map.map Added) t1 t2


emitHashMap :: Handle -> IO (Map ByteString (Set ByteString))
emitHashMap h = P.runEffect $ (P.fold updateMap Map.empty id (pipeHashLine (PB.fromHandle h)))

-- updatemap
updateMap :: Map ByteString (Set ByteString) -> (ByteString, ByteString) -> Map ByteString (Set ByteString)
updateMap m (h, f) = Map.insertWith (Set.union) h (Set.singleton f) m

-- Hash line pipe
pipeHashLine :: P.MonadIO m => Producer ByteString m () -> Producer (ByteString, ByteString) m ()
pipeHashLine producer = do
    (result, rest) <- P.lift $ runStateT (PA.parse hashLine) producer

    case result of
        Nothing -> return ()
        Just r  -> do
            case r of
                Right m -> P.yield m
                Left e  -> P.liftIO $ putStrLn ("Error: " ++ show e)

            -- Keep going
            pipeHashLine rest


-- Hash parser
asciiToWord8 :: Char -> Word8
asciiToWord8 = fromIntegral . ord

wSpace :: Word8
wSpace = asciiToWord8 ' '

wTab :: Word8
wTab = asciiToWord8 '\t'

spaces :: Parser ()
spaces = skip (\w -> w == wSpace
                  || w == wTab)

wLF :: Word8
wLF = asciiToWord8 '\n'

lf :: Parser ()
lf = skip ((==) wLF)

hashLine :: Parser (ByteString, ByteString)
hashLine = (,) <$> (takeWhile1 ((/=) wSpace) <* many spaces) <*> (takeWhile1 ((/=) wLF) <* lf)


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
