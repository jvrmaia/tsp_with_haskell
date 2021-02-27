module ListaPrioridade where
import System.Environment 
import PairHeap
import Point

type AuxList = (Point,Dist,Point)
type ListaPrioridade = PHeap AuxList

comp :: AuxList -> AuxList -> Ordering
comp (_,a,_) (_,b,_)
	|a > b = GT
	|a == b = EQ
	|a < b = LT

selectMostFar :: ListaPrioridade -> ((Point,Point),ListaPrioridade)
selectMostFar xs = let ((a,_,b),xs') = pheapDelMax comp xs
	in ((a,b),xs')

null :: ListaPrioridade -> Bool
null = (Empty==)

initPointsNotInTour :: Point -> Point -> [Point] -> ListaPrioridade
initPointsNotInTour x0 x1 [] = Empty
initPointsNotInTour x0 x1 (y:ys)
			|y==x1 || y==x0 = initPointsNotInTour x0 x1 ys
			|dist y x1 < dist y x0 = PairHeap.insert comp (y,dist y x1,x1) $ initPointsNotInTour x0 x1 ys 
			|otherwise = PairHeap.insert comp (y,dist y x0,x0) $ initPointsNotInTour x0 x1 ys

-- insert point pointmostnear partiallist
insert :: Point -> Point -> ListaPrioridade -> ListaPrioridade
insert a b xs = PairHeap.insert comp (a,dist a b,b) xs

