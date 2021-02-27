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


tourlen tour = foldr (+) 0 (alldists (tour++[(head tour)]))

alldists :: (Floating b) => [Point Int] -> [b]
alldists xs = map dist (zip xs (tail xs))

