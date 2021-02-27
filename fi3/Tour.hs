module Tour where
import System.Environment 
import Point
import KDTree

type AuxTour = (Point,Point,Point,Dist) -- anterior, atual, posterior, dist atual prox
type Tour = KDTree AuxTour
toPoint :: AuxTour -> Point
toPoint (_,a,_,_) = a

--Insere um ponto num tour, observando se esse ponto deve ser inserido antes ou depois do ponto de maior distância ao tour
insertPointInTour :: (Point,Point) -> Tour -> Tour
insertPointInTour (a,b) tour =
		case search toPoint (b,b,b,1.0) tour of
			Just (an,at,pos,d) -> 
				if (dist an at + dist at a + dist a pos) < (dist an a + dist a at + dist at pos)  
				then case search toPoint (pos,pos,pos,1.0) tour of
					Just (an2,at2,pos2,d2) -> KDTree.kdtIns toPoint (a,pos,pos2, dist pos pos2) $ KDTree.kdtIns toPoint (an,at,a,dist at a) $ KDTree.kdtIns toPoint (at,a,pos,dist a pos) tour
				else case search toPoint (an,an,an,1.0) tour of
					Just (an2,at2,pos2,d2) -> KDTree.kdtIns toPoint (an2,an,a,dist an a) $ KDTree.kdtIns toPoint (an,a,at,dist at a) $ KDTree.kdtIns toPoint (a,at,pos,dist at pos) tour

initTour :: Point -> Point -> [Point] -> Tour
initTour x0 x1 xs = KDTree.kdtIns toPoint (x0,x1,x0,dist x0 x1)(KDTree.kdtIns toPoint (x1,x0,x1,dist x0 x1) (KDTree.kdtBuild xs))

nn :: Point -> Tour -> Point
nn x0 tour = kdtNN toPoint x0 tour

toList :: Tour -> [Point]
toList tour = toListLoop a b tour
	where 
		(a,b)= pegamin tour

		pegamin (KDTree.Leaf (_,a,b,_)) = (a,b)
		pegamin (KDTree.Node median _ lnode rnode) = pegamin lnode

		toListLoop :: Point -> Point -> Tour -> [Point]
		toListLoop a c tour
			|a==c = [a]
			|otherwise =  c:toListLoop a m tour
			where 
				Just (_,_,m,_) = search toPoint (c,c,c,1.0) tour 

