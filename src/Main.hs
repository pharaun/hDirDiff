import Control.Applicative
import Control.Monad.Trans.State.Strict
import Data.Char
import System.IO (withFile, IOMode(ReadMode), Handle)

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

import Pipes ((>->), Producer)
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
    srcMap <- withFile src ReadMode emitHashMap
    dstMap <- withFile dst ReadMode emitHashMap

    -- Compare the two map and create a "final map"
    putStrLn "Hi"

emitHashMap :: Handle -> IO (Map ByteString [ByteString])
emitHashMap h = do
    P.runEffect $ (pipeHashLine (PB.fromHandle h) >-> P.map (\(a, b) -> (BS.intercalate (C8.pack " : ") [a, b]) `BS.append` (C8.pack "\n")) >-> PB.stdout)
    return Map.empty


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



updateMap :: Map ByteString [ByteString] -> (ByteString, ByteString) -> Map ByteString [ByteString]
updateMap m (h, f) = Map.insertWith' (++) h [f] m

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
