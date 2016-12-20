module Lib
    ( compute
    , parse
    ) where

import           Control.Concurrent.ParallelIO.Global (parallel, stopGlobalPool)
import           Control.Monad                        (forM, replicateM)
import           System.Environment                   (getArgs)
import           System.Random.MWC                    (createSystemRandom, uniformR)

import           Oth                                  (Board, Index, Turn, bl, count, em, fromList,
                                                       opp, place, placeables, wh)

parse :: IO (Board, Turn, Int)
parse = getArgs >>= \[b, t, n] -> return (fromList $ map parse' $ read b, parse' t, read n)
  where
    parse' "B" = bl
    parse' "W" = wh
    parse' "E" = em

compute :: Board -> Turn -> Int -> IO Index
compute b t n = do
    g <- createSystemRandom
    r <- parallel $ map (\i -> placeRandomlyToEndN (place b i t) (opp t) n) $ placeables b t
    stopGlobalPool
    return $
        placeables b t !! maxIndex (map (length . filter ((>) <$> countF t <*> countF (opp t))) r)
  where
    countF = flip count
    maxIndex l = snd . maximum $ zip l [0 .. ]

placeRandomlyToEndN :: Board -> Turn -> Int -> IO [Board]
placeRandomlyToEndN b t n = createSystemRandom >>= replicateM n . placeRandomlyToEnd b t
  where
    placeRandomlyToEnd board turn g
        | null placeablesList && null (placeables board nt) = return board
        | null placeablesList = placeRandomlyToEnd board nt g
        | otherwise = sample placeablesList g >>= \i -> placeRandomlyToEnd (place board i nt) nt g
      where
        nt = opp turn
        placeablesList = placeables board turn
    sample l g = (l !!) <$> uniformR (0, length l - 1) g
