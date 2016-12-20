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
placeRandomlyToEndN b t n = createSystemRandom >>= \g -> replicateM n $ placeRandomlyToEnd g b t
  where
    placeRandomlyToEnd gen board turn
        | null placeablesList && null (placeables board nt) = return board
        | null placeablesList = placeRandomlyToEnd gen board nt
        | otherwise = sample placeablesList >>= \i -> placeRandomlyToEnd gen (place board i nt) nt
      where
        nt = opp turn
        placeablesList = placeables board turn
        sample l = (l !!) <$> uniformR (0, length l - 1) gen
