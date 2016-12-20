module Main where

import           Lib (compute, parse)

main :: IO ()
main = parse >>= (\(b, t, n) -> compute b t n) >>= print
