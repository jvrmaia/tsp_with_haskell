{-# OPTIONS_GHC -fglasgow-exts #-}
module Point where
import System.IO

type Dist = Double
type Point = (Double,Double)

dist :: Point -> Point -> Dist
dist (x0,y0) (x1,y1) = sqrt $ (x0-x1)*(x0-x1) + (y0-y1)*(y0-y1)

xcoord :: Point -> Double
xcoord (x,_)=x
ycoord :: Point -> Double
ycoord(_,y)=y

parseDouble :: (ReadS Double) = reads
parseFloat :: (ReadS Float) = reads
parseInt :: (ReadS Int) = reads

readInt :: String -> IO Int
readInt = return . fst . head . parseInt

readPoint :: String -> (Double,Double)
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




