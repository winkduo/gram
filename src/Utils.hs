module Utils (mapBy, mapByMaybe) where

import qualified Data.Map as M
import Control.Arrow ((&&&))

mapBy :: Ord k => (a -> k) -> [a] -> M.Map k a
mapBy to_key = M.fromList . map (to_key &&& id)

mapByMaybe :: Ord k => (a -> Maybe k) -> [a] -> M.Map k [a]
mapByMaybe to_key = flip foldr M.empty $ \a ->
  case to_key a of
    Nothing ->
      id
    Just k ->
      M.alter (Just . maybe [] (a:)) k
