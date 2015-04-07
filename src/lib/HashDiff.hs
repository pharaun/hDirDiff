module HashDiff
    ( foldlMap
    , findMovedFiles
    , Action(..)
    , optionalIntersectionWith
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

import Pipes.ByteString (ByteString)

-- Actions to perform
data Action = Removed (Set ByteString) -- Remove
            | Added (Set ByteString) -- Local file to Upload
            | Moved ByteString (Set ByteString) (Set ByteString) -- Src to copy from, added files (to copy to), removed files (to remove afterward)
            deriving (Show, Eq)


-- | Extended version of 'intersectionWith' that enables dropping of
-- elements if the combining function returns a 'Nothing'
--
--optionalIntersectionWith :: Ord k => (a -> b -> Maybe c) -> Map k a -> Map k b -> Map k c
optionalIntersectionWith :: Ord k => (Set ByteString -> Set ByteString -> Maybe Action) -> Map k (Set ByteString) -> Map k (Set ByteString) -> Map k Action
optionalIntersectionWith func initial final = Map.mergeWithKey (\_ i f -> func i f) (Map.map Removed) (Map.map Added) initial final


findMovedFiles :: Set ByteString -> Set ByteString -> Maybe Action
findMovedFiles initial final
    -- 1. Compare file list, if same then move on
    | initial == final                              = Nothing

    -- 2. If initial is empty and final is not, we've added files
    | Set.null initial && (not $ Set.null final)    = Just $ Added final

    -- 3. If initial is not empty and final is, we've removed files
    | (not $ Set.null initial) && Set.null final    = Just $ Removed initial

    -- 4. Both initial and final state are not empty or the same.
    --  Check if there's anything to copy to, if not its a removal (excess files)
    --  Else it is a move with possibly some extra new file and excess files
    | otherwise =
        let new = Set.difference final initial
            removed = Set.difference initial final
            copyFrom = Set.elemAt 0 initial
            copyTo = Set.delete copyFrom new

        in if Set.null copyTo
           then Just $ Removed removed
           else Just $ Moved copyFrom copyTo removed


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
