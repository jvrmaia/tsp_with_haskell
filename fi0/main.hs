{-# OPTIONS_GHC -fglasgow-exts #-}
module Main where
import System.Environment
import Data.List

import Point
import Prototipo

getPoints args =
   if (length args == 0)
   then stdinReadPoints
   else readPoints (args!!0)  

main =
    getArgs >>= 
    getPoints >>=
    tsp >>=
    mapM_ printf 

printf :: Point -> IO()
printf (a,b) = putStr $ show(a)++" "++show(b)++"\n"
