module Main where

import Graph (evaluateGraph)

main :: IO ()
main = evaluateGraph "(c and (not f) or x) or (t xor f and (r)) "
