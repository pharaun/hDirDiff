import Control.Concurrent.Async
import Control.Applicative
import Control.Monad.Trans.State.Strict
import Data.Char
import System.IO (withFile, IOMode(ReadMode), Handle)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.ByteString as BS

import Pipes (Producer)
import qualified Pipes as P
import qualified Pipes.Prelude as P
import qualified Pipes.Attoparsec as PA
import Pipes.ByteString (Word8, ByteString)
import qualified Pipes.ByteString as PB

import Data.Attoparsec.ByteString (Parser, skip, takeWhile1)

import Options.Applicative hiding (Parser)


main = do
    (src, dst) <- getArgs

    -- Handle source hash
    (srcMap, dstMap) <- concurrently (withFile src ReadMode emitHashMap) (withFile dst ReadMode emitHashMap)

    -- 1. Identify files that has been removed from the destination
    --      (check hashes that are not present)
    let removed = Map.difference srcMap dstMap

    -- 2. Identify files that has been added to the destination
    --      (check hashes that are new)
    let new = Map.difference dstMap srcMap

    -- 3. Identify moved files
    let moved = optionalIntersectionWith findMovedFiles srcMap dstMap

    -- 4. Emit changed list (added/removed/moved)


    -- Compare the two map and create a "final map"
    putStrLn "Hi"

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
    | otherwise                             = Just $ Moved BS.empty (Set.union src dst) (Set.union src dst) -- TODO: wrong code here


data Action = Removed (Set ByteString)
            | Added (Set ByteString)
            | Moved ByteString (Set ByteString) (Set ByteString) -- TODO: fix this (Should be 1. src to copy from 2. list of dst to copy to 3. src to remove)
            deriving Show


-- | Extended version of 'intersectionWith' that enables dropping of
-- elements if the combining function returns a 'Nothing'
optionalIntersectionWith :: Ord k => (a -> b -> Maybe c) -> Map k a -> Map k b -> Map k c
optionalIntersectionWith f t1 t2 = Map.mergeWithKey (\_ x1 x2 -> f x1 x2) (const Map.empty) (const Map.empty) t1 t2


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
