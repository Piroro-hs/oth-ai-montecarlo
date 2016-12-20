module Lib
    ( compute
    , parse
    ) where

import           Control.Concurrent.ParallelIO.Global (parallel, stopGlobalPool)
import           Control.Monad                        (replicateM)
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
    r <- parallel $ map (\i -> placeRandomlyToEndN (place b i t) (opp t) n) pl
    stopGlobalPool
    return $ pl !! maxIndex (map (length . filter ((>) <$> (`count` t) <*> (`count` opp t))) r)
  where
    maxIndex l = snd . maximum $ zip l [0 .. ]
    pl = placeables b t

placeRandomlyToEndN :: Board -> Turn -> Int -> IO [Board]
placeRandomlyToEndN b t n = createSystemRandom >>= replicateM n . placeRandomlyToEnd b t
  where
    placeRandomlyToEnd board turn g
        | null pl && null (placeables board nt) = return board
        | null pl = placeRandomlyToEnd board nt g
        | otherwise = sample pl g >>= \i -> placeRandomlyToEnd (place board i turn) nt g
      where
        nt = opp turn
        pl = placeables board turn
    sample l g = (l !!) <$> uniformR (0, length l - 1) g
