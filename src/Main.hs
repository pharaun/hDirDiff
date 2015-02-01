import Control.Concurrent.Async
import Control.Applicative
import Control.Monad.Trans.State.Strict
import Data.Char
import System.IO (withFile, IOMode(ReadMode), Handle)

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

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

    -- Several steps to be done:
    -- 1. Identify files that has been removed from the destination
    --      (check hashes that are not present)
    -- 2. Identify files that has been added to the destination
    --      (check hashes that are new)
    -- 3. Identify moved files
    --      a. Compare file list, if same then move on
    --      b. If src is not empty and dst is empty, we removed files copies
    --      c. If dst is not empty and src is empty, we added file copies
    --      d. If dst and source is not empty, we have some sort of moves
    --          (Since all source and all dest is the same, can probably make
    --              it simpler and just do multiple copies from one
    --              source/dest to the rest)
    -- 4. Emit changed list (added/removed/moved)


    -- Compare the two map and create a "final map"
    putStrLn "Hi"

emitHashMap :: Handle -> IO (Map ByteString [ByteString])
emitHashMap h = P.runEffect $ (P.fold updateMap Map.empty id (pipeHashLine (PB.fromHandle h)))

-- updatemap
-- TODO: probably want to make that [] list into a Set
updateMap :: Map ByteString [ByteString] -> (ByteString, ByteString) -> Map ByteString [ByteString]
updateMap m (h, f) = Map.insertWith' (++) h [f] m

-- Hash line pipe
pipeHashLine :: P.MonadIO m => Producer ByteString m () -> Producer (ByteString, ByteString) m ()
pipeHashLine producer = do
    (result, rest) <- P.lift $ runStateT (PA.parse hashLine) producer

    case result of
        Nothing -> P.liftIO $ putStrLn "Pipe is exhausted\n"
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
