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
