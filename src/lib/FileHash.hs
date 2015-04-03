module FileHash
    ( emitHashMap
    , updateMap
    , pipeHashLine
    , wSpace
    , wTab
    , spaces
    , wLF
    , lf
    , hashLine
    ) where

import Control.Applicative
import Control.Monad.Trans.State.Strict
import Data.Char
import System.IO (Handle)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Pipes (Producer)
import qualified Pipes as P
import qualified Pipes.Prelude as P
import qualified Pipes.Attoparsec as PA
import Pipes.ByteString (Word8, ByteString)
import qualified Pipes.ByteString as PB

import Data.Attoparsec.ByteString (Parser, skip, takeWhile1, endOfInput)




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
hashLine = (,) <$> (takeWhile1 ((/=) wSpace) <* many spaces) <*> (takeWhile1 ((/=) wLF) <* (lf <|> endOfInput))
