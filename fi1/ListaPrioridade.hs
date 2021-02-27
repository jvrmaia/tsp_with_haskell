module ListaPrioridade where
import System.Environment 
import BinomialHeap
import Point

type AuxList = (Point,Dist,Point)
type ListaPrioridade = BinomialHeap AuxList

comp :: AuxList -> AuxList -> Ordering
comp (_,a,_) (_,b,_)
	|a > b = GT
	|a == b = EQ
	|a < b = LT

selectMostFar :: ListaPrioridade -> ((Point,Point),ListaPrioridade)
selectMostFar xs = let ((a,_,b),x) = bheapDelMax comp xs
	in ((a,b),x)

null :: ListaPrioridade -> Bool
null = ([]==)

initPointsNotInTour :: Point -> Point -> [Point] -> ListaPrioridade
initPointsNotInTour x0 x1 [] = []
initPointsNotInTour x0 x1 (y:ys)
			|y==x1 || y==x0 = initPointsNotInTour x0 x1 ys
			|dist y x1 < dist y x0 = BinomialHeap.insert comp (y,dist y x1,x1) $ initPointsNotInTour x0 x1 ys 
			|otherwise = BinomialHeap.insert comp (y,dist y x0,x0) $ initPointsNotInTour x0 x1 ys

-- insert point pointmostnear partiallist
insert :: Point -> Point -> ListaPrioridade -> ListaPrioridade
insert a b xs = BinomialHeap.insert comp (a,dist a b,b) xs

