{-# OPTIONS_GHC -fglasgow-exts #-}
module Main where
import System.Environment
import Dijkstra
import Point

getPoints args =
   if (length args == 0)
   then stdinReadPoints
   else readPoints (args!!0)  

main =
    getArgs >>= 
    getPoints >>= 
    dijkstra >>=
    tourFromEuller >>=
    mapM_ printf

printf :: Point -> IO()
printf (a,b) = putStr $ show(a)++" "++show(b)++"\n"

tourFromEuller :: (Monad m) => Mst -> m [Point]
tourFromEuller mst = return $ tour mst
	where 
		tour ::  Mst -> [Point]
		tour Empty = []
		tour (Mst pt [] _) = [pt]
		tour (Mst pt (q:qs) _) = pt:(tour' (q:qs))
		
		tour' ::  [Mst] -> [Point]
		tour' [q] = tour q
		tour' (q:qs) = (tour q) ++ (tour' qs)

