module Oth
    ( Board
    , Dir
    , Index
    , Piece
    , Turn
    , bl
    , wh
    , em
    , board
    , count
    , flipPiece
    , fromList
    , get
    , initBoard
    , opp
    , place
    , placeable
    , placeables
    , set
    , toVector
    ) where

import           Data.Ix             (inRange)
import qualified Data.Vector.Unboxed as VU

type Dir = (Int, Int)
type Index = (Int, Int)

dirs :: [Dir]
dirs = filter (/= (0, 0)) $ map (,) [-1, 0, 1] <*> [-1, 0, 1]

type Piece = Int
type Turn = Piece

newtype Board = Board { toVector :: VU.Vector Piece } deriving (Show)

bl = 1
wh = -1
em = 0

opp :: Piece -> Piece
opp p
    | p == bl = wh
    | p == wh = bl
    | otherwise = em

board :: Board
board = Board $ VU.replicate 64 em

fromList :: [Piece] -> Board
fromList = fst . foldl (\(b, i) p -> (set b (i `div` 8, i `mod` 8) p, i + 1)) (board, 0)

get :: Board -> Index -> Piece
get b (r, c) = toVector b `VU.unsafeIndex` (r * 8 + c)

set :: Board -> Index -> Piece -> Board
set b (r, c) p = Board $ toVector b `VU.unsafeUpd` [(r * 8 + c, p)]

count :: Board -> Piece -> Int
count b p = VU.length . VU.filter (== p) $ toVector b

flipPiece :: Board -> Index -> Board
flipPiece b i = set b i $ opp $ get b i

initBoard :: Board
initBoard = set (set (set (set board (4, 3) bl) (3, 4) bl) (4, 4) wh) (3, 3) wh

place :: Board -> Index -> Piece -> Board
place b i p = foldl (`place'` i) (set b i p) $ filter (placeableForDir b i p) dirs
  where
    place' b (r, c) di@(dr, dc)
        | get b ni == p = b
        | otherwise = set (place' b ni di) ni p
      where
        ni = (r + dr, c + dc)

placeable :: Board -> Index -> Turn -> Bool
placeable b i t = get b i == em && any (placeableForDir b i t) dirs

placeableForDir :: Board -> Index -> Turn -> Dir -> Bool
placeableForDir b i t = placeable' False . tail . takeToEnd i
  where
    takeToEnd index@(r, c) di@(dr, dc)
        | not (inRange (0, 7) r && inRange (0, 7) c) = []
        | otherwise = get b index : takeToEnd (r + dr, c + dc) di
    placeable' _ [] = False
    placeable' f (p:xs)
        | p == t = f
        | p == opp t = placeable' True xs
        | otherwise = False

placeables :: Board -> Turn -> [Index]
placeables b t = filter (($ t) . placeable b) $ map (\i -> (i `div` 8, i `mod` 8)) [0 .. 63]
