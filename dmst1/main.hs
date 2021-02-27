{-# OPTIONS_GHC -fglasgow-exts #-}
module Main where
import System.Environment

import Point
import Dijkstra

getPoints args =
   if (length args == 0)
   then stdinReadPoints
   else readPoints (args!!0)  

main =
    getArgs >>= 
    getPoints >>= 
    dijkstra >>=
    tourgrande >>=
    mapM_ printf 

printf :: Point -> IO()
printf (a,b) = putStr $ show(a)++" "++show(b)++"\n"

tourgrande :: (Monad m) => Mst -> m [Point]
tourgrande mst = return $ tour mst
	where 
		tour ::  Mst -> [Point]
		tour Empty = []
		tour (Mst pt [] _) = [pt]
		tour (Mst pt (q:qs) r) = pt:(tour q)++(tour (Mst pt qs r))


