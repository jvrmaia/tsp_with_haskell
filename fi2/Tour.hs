module Tour where
import System.Environment 
import Point
import BTree
import KDTree2

type AuxTour = (Point,Point,Point,Dist) -- anterior, atual, posterior, dist atual prox
type Tour = (BTree AuxTour,KDTree)

comp (_,a,_,_) (_,b,_,_)
	|a < b = LT
	|a == b = EQ
	|a > b = GT

--Insere um ponto num tour, observando se esse ponto deve ser inserido antes ou depois do ponto de maior distância ao tour
insertPointInTour :: (Point,Point) -> Tour -> Tour
insertPointInTour (a,b) (tour,nnaux) =
		case search comp (b,b,b,1.0) tour of
			Just (an,at,pos,d) -> 
				if (dist an at + dist at a + dist a pos) < (dist an a + dist a at + dist at pos)  
				then case search comp (pos,pos,pos,1.0) tour of
					Just (an2,at2,pos2,d2) -> (insert comp (a,pos,pos2, dist pos pos2) $ insert comp (an,at,a,dist at a) $ insert comp (at,a,pos,dist a pos) tour, kdtIns a nnaux)
				else case search comp (an,an,an,1.0) tour of
					Just (an2,at2,pos2,d2) -> (insert comp (an2,an,a,dist an a) $ insert comp (an,a,at,dist at a) $ insert comp (a,at,pos,dist at pos) tour , kdtIns a nnaux)

initTour :: Point -> Point -> [Point] -> Tour
initTour x0 x1 xs = (insert comp (x0,x1,x0,dist x0 x1)(insert comp (x1,x0,x1,dist x0 x1) BTree.Leaf),kdtIns x0 $ kdtIns x1 $ kdtBuild xs)

nn :: Point -> Tour -> Point
nn x0 (tour,nnaux) = 
	case nnaux of
		DeadLeaf _ -> error "Inferno"
		DeadNode _ _ _ _ -> error "Inferno"
		_ ->kdtNN x0 nnaux					

toList :: Tour -> [Point]
toList (tour,_) = 
	case tour of
		BTree.Leaf -> []
		Node2 _ (_,a,b,_) _ -> toListLoop a b tour 
		Node3 _ (_,a,b,_) _ _ _ -> toListLoop a b tour 
	where 
		toListLoop :: Point -> Point -> BTree AuxTour -> [Point]
		toListLoop a c tour
			|a==c = [a]
			|otherwise =  c:toListLoop a m tour
			where 
				Just (_,_,m,_) = search comp (c,c,c,1.0) tour 

