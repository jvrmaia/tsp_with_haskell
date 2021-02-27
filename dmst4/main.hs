{-# OPTIONS_GHC -fglasgow-exts #-}
module Main where
import System.Environment
import Kruskall
import Point
import Data.List(union)

getPoints args =
   if (length args == 0)
   then stdinReadPoints
   else readPoints (args!!0)  

main =
    getArgs >>= 
    getPoints >>= 
    kruskall >>=
    tourFromEuller >>=
    mapM_ printf

printf :: Point -> IO()
printf (a,b) = putStr $ show(a)++" "++show(b)++"\n"

tourFromEuller :: (Monad m) => [Point] -> m [Point]
tourFromEuller mst = return $ Data.List.union [] mst
