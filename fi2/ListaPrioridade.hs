module ListaPrioridade where
import System.Environment 
import BTree2
import Point

type AuxList = (Point,Dist,Point)
type ListaPrioridade = BTree AuxList

comp :: AuxList -> AuxList -> Ordering
comp (_,a,_) (_,b,_)
	|a > b = GT
	|a == b = EQ
	|a < b = LT

initPointsNotInTour :: Point -> Point -> [Point] -> ListaPrioridade
initPointsNotInTour x0 x1 [] = Leaf
initPointsNotInTour x0 x1 (y:ys)
			|y==x1 || y==x0 = initPointsNotInTour x0 x1 ys
			|dist y x1 < dist y x0 = BTree2.insert comp (y,dist y x1,x1) $ initPointsNotInTour x0 x1 ys 
			|otherwise = BTree2.insert comp (y,dist y x0,x0) $ initPointsNotInTour x0 x1 ys

selectMostFar :: ListaPrioridade -> ((Point,Point),ListaPrioridade)
selectMostFar xs = let ((a,_,b),xs') = deleteMax xs
	in ((a,b),xs')

null :: ListaPrioridade -> Bool
null = (Leaf==)

insert :: Point -> Point -> ListaPrioridade -> ListaPrioridade
insert a b xs = BTree2.insert comp (a,dist a b,b) xs

