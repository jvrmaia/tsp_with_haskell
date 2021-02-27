module Main where

-- next line imports module Point
import Point

-- program execution starts by calling function main, defined next

{-                                  main  
   'main' does
          calls 'readIntpts' to read a list of  (point Int)
          calls 'alldists' to evaluate the square of the distance between
                           every pair of points in the list received from
                           readIntPts
          calls putStrLn to output the sum of distances
-}

main = readIntPts [] >>= \tour -> putStrLn (show (tourlen tour)) 


tourlen (t:ts) = distsum (ts++[t]) (t,0)
  where distsum [] (_,s) = s
	distsum (t:ts) (t0,s) = distsum ts (t,dist(t0,t)+s)


