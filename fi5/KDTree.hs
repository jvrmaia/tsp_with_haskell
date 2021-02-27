{-# OPTIONS_GHC -fglasgow-exts #-}
module KDTree where
import Data.List
import Point

type Dimen = Point -> Double
data KDTree b =
        Leaf b
        |DeadLeaf Point
	|Node Point Dimen (KDTree b) (KDTree b)
	|DeadNode Point Dimen (KDTree b) (KDTree b)

data InsOK b =
	OK (KDTree b)
	|NOK

kdtBuild [pt] = DeadLeaf pt
kdtBuild [pt0,pt1]-- como default p/ 2 nums sempre por y!!!
	|ycoord pt0 <= ycoord pt1 = DeadNode pt0 ycoord (DeadLeaf pt0) (DeadLeaf pt1)
	|otherwise = DeadNode pt1 ycoord (DeadLeaf pt1) (DeadLeaf pt0)
kdtBuild (p0:p1:p2:pts) = DeadNode md ld (kdtBuild $ mi:md:[x|x<-(pts),ld x<=ld md]) (kdtBuild $ ma:[x|x<-(pts),ld x>ld md])
	where
    		ld
			|(xmax-xmin)<(ymax-ymin) = ycoord
			|otherwise = xcoord
			where 
				((xmin,ymin),(xmax,ymax)) = foldr xyminmax (p0,p0) (p1:p2:pts)
				xyminmax :: Point -> (Point,Point) -> (Point,Point)
				xyminmax (xp,yp) ((xmi,ymi),(xma,yma)) = ((min xmi xp,min ymi yp),(max xma xp,max yma yp))

        	(mi,md,ma) = ordena [p0,p1,p2]
		ordena :: [Point] -> (Point,Point,Point)
		ordena [p0,p1,p2]
			|ld p0<=ld p1 && ld p1<=ld p2 = (p0,p1,p2)
			|ld p0<=ld p2 && ld p2<=ld p1 = (p0,p2,p1)
			|ld p1<=ld p0 && ld p0<=ld p2 = (p1,p0,p2)
			|ld p1<=ld p2 && ld p2<=ld p0 = (p1,p2,p0)
			|ld p2<=ld p0 && ld p0<=ld p1 = (p2,p0,p1)
			|ld p2<=ld p1 && ld p1<=ld p0 = (p2,p1,p0)

search topoint _ (DeadLeaf _) = Nothing
search topoint _ (DeadNode _ _ _ _) = Nothing
search topoint x  (Leaf y)
	|topoint x == topoint y = Just y
	|otherwise = Nothing
search topoint x (Node median dim lnode rnode) 
	| dim pt < dim median = search topoint x lnode
	| dim pt == dim median = 
		case search topoint x lnode of
			Nothing -> search topoint x rnode
			Just p -> Just p
	| otherwise = search topoint x rnode
	where pt = topoint x

kdtIns :: (b -> Point) -> b -> KDTree b -> KDTree b
kdtIns topoint b tree =
	case reckdtIns topoint b tree of
		OK t -> t
		NOK -> tree
reckdtIns :: (b -> Point) -> b -> KDTree b -> InsOK b
reckdtIns topoint x (Leaf y)
	|topoint x==topoint y = OK (Leaf x)
	|otherwise = NOK
reckdtIns topoint x (DeadLeaf pt)
	| topoint x == pt = OK (Leaf x)
	| otherwise = NOK
reckdtIns topoint x a@(Node median dim lnode rnode)
	| dim pt < dim median = 
		case reckdtIns topoint x lnode of
			OK new -> OK (Node median dim new rnode)
			NOK -> NOK
	| dim pt == dim median =
		case reckdtIns topoint x lnode of
			OK new  -> OK (Node median dim new rnode)
			NOK -> case reckdtIns topoint x rnode of
				OK new -> OK (Node median dim lnode new)
				NOK -> NOK
	| otherwise =
		case reckdtIns topoint x rnode of
			OK new -> OK (Node median dim lnode new)
			NOK -> NOK
	where pt = topoint x
reckdtIns topoint x a@(DeadNode median dim lnode rnode)
	| dim pt < dim median = 
		case reckdtIns topoint x lnode of
			OK new  -> OK (Node median dim new rnode)
			NOK -> NOK
	| dim pt == dim median =
		case reckdtIns topoint x lnode of
			OK new  -> OK (Node median dim new rnode)
			NOK -> case reckdtIns topoint x rnode of
				OK new  -> OK (Node median dim lnode new)
				NOK -> NOK
	| otherwise =
		case reckdtIns topoint x rnode of
			OK new -> OK (Node median dim lnode new)
			NOK -> NOK
	where pt = topoint x

accFInBTree f acc b0 (DeadLeaf _) = b0
accFInBTree f acc b0 (DeadNode _ _ _ _) = b0
accFInBTree f acc b0 (Leaf p) = f p
accFInBTree f acc b0 (Node median dim lnode rnode) = acc (accFInBTree f acc b0 lnode) (accFInBTree f acc b0 lnode)

kdtNN :: (b -> Point) -> Point -> KDTree b -> Point
kdtNN topoint _ (Leaf pt) = topoint pt
kdtNN topoint pt (Node median dim lnode rnode)
	|vazio lnode = kdtNN topoint pt rnode
	|vazio rnode = kdtNN topoint pt lnode
	| dim pt <= dim median, nn <- kdtNN topoint pt lnode , d <- dist nn pt =
		if dim pt + d > dim median
		then let nn2 = kdtNN topoint pt rnode
			in if dist pt nn2 < d
			then nn2
			else nn
		else nn
	|otherwise, nn <- kdtNN topoint pt rnode , d <- dist nn pt=
		if dim pt - d < dim median
		then let nn2 = kdtNN topoint pt lnode
			in if dist pt nn2 < d
			then nn2
			else nn
	         else nn
	where
		vazio tree = case tree of
			Leaf _ -> False
			DeadLeaf _ -> True
			DeadNode _ _ _ _ -> True
			Node _ _ _ _ -> False

