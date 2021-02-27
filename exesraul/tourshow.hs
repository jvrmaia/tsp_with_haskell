module Main where

import Graphics.HGL
import System.IO

printtour tour = print tour
poly :: [Point] -> IO ()
poly tour = runGraphics $
		withWindow_ "TSP Tour Window" (1000, 700) $ \ w -> 
		do
	  	 drawInWindow w $ text (100, 100) "TSP Tour"
          	 drawInWindow w $ polyline (mapToWindow (980,680) tour)
	  	 getKey w

mapToWindow :: Point->[Point]->[Point]
mapToWindow (xw,yw) tour = 
	let xfact = (fromIntegral (maximum (map fst tour))/fromIntegral xw)
            yfact = (fromIntegral (maximum (map snd tour))/fromIntegral yw)
        in map (\ (x,y) -> (truncate (fromIntegral x/xfact),
                            truncate (fromIntegral y/yfact)))
		  tour


main = readPoints [] >>= \tour ->  poly (tour++[head tour]) 


readPoints ::  [String] -> IO [Point]


readPoints xs = isEOF >>= \eof -> if eof
                                  then return (map read (reverse xs))
                                  else getLine >>= \ln -> readPoints (ln:xs)


