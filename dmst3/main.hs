{-# OPTIONS_GHC -fglasgow-exts #-}
module Main where
import System.Environment

import Dijkstra
import Point
import Data.List(union)

getPoints args =
   if (length args == 0)
   then stdinReadPoints
   else readPoints (args!!0)  

main =
    getArgs >>= 
    getPoints >>= 
    dijkstra >>=
    mapM_ printf

printf :: Point -> IO()
printf (a,b) = putStr $ show(a)++" "++show(b)++"\n"

