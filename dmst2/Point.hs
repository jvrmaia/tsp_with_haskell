{-# OPTIONS_GHC -fglasgow-exts #-}
module Point where
import System.IO
import Data.Bits

type Point = (Double, Double)
type Dist = Double

dist :: Point -> Point -> Dist
dist (x0,y0) (x1,y1) = sqrt $ (x0-x1)**2 + (y0-y1)**2

parseDouble :: (ReadS Double) = reads
parseFloat :: (ReadS Float) = reads
parseInt :: (ReadS Int) = reads

readInt :: String -> IO Int
readInt = return . fst . head . parseInt

readPoint :: String -> Point

readPoint str =
    let (x, str0) = head . parseDouble $ str
        (y, _) = head . parseDouble $ str0
    in (x,y)

readPoints file = 
    readFile file >>=
    return . lines >>=
    return . map readPoint

stdinReadPoints = 
    hGetContents stdin >>=
    return . lines >>=
    return . map readPoint

xcoord (x,_) = x
ycoord (_,y) = y

