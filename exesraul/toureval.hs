module Main where

-- next line imports module Point
import Point
import TourEval
import List
import Control.Exception
import System.CPUTime

-- program execution starts by calling function main, defined next

{-                                  main  
   'main' does
          calls 'readIntpts' to read a list of  (point Int)
          calls 'alldists' to evaluate the square of the distance between
                           every pair of points in the list received from
                           readIntPts
          calls putStrLn to output the sum of distances
-}

main = readIntPts [] >>= \pts -> let tour = toureval pts
                                 in putStr (showPoints (tourOK pts tour))
{-
main = do
	pts <- readIntPts [] 
        t0 <- getCPUTime
	tour <- return (toureval pts)
        t1 <- getCPUTime
        putStr (showPoints (tourOK pts tour))
        putStrLn ("Run Time: "++(show (time (t1-t0)))++"s")

time t = ((fromInteger t)/(10^12))
-}

tourOK pts tour = assert (sort pts == sort tour) tour
