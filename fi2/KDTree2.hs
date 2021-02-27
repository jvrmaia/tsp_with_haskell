{-# OPTIONS_GHC -fglasgow-exts #-}
module KDTree2 where
import Data.List
import Point

type Dimen = Point -> Double
data KDTree =
        Leaf Point
        |DeadLeaf Point
	|Node Point Dimen KDTree KDTree
	|DeadNode Point Dimen KDTree KDTree

data InsOK =
	OK (KDTree)
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

kdtNN :: Point -> KDTree -> Point
kdtNN _ (Leaf pt) = pt
kdtNN pt (Node median dim lnode rnode)
	|vazio lnode = kdtNN pt rnode
	|vazio rnode = kdtNN pt lnode
	| dim pt <= dim median, nn <- kdtNN pt lnode , d <- dist nn pt =
		if dim pt + d > dim median
		then let nn2 = kdtNN pt rnode
			in if dist pt nn2 < d
			then nn2
			else nn
		else nn
	|otherwise, nn <- kdtNN pt rnode , d <- dist nn pt=
		if dim pt - d < dim median
		then let nn2 = kdtNN pt lnode
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

kdtIns :: Point -> KDTree -> KDTree
kdtIns pt tree =
	case reckdtIns pt tree of
		OK t -> t
		NOK -> tree

reckdtIns :: Point -> KDTree -> InsOK
reckdtIns (pt) (Leaf lpt)
	| pt == lpt = OK (Leaf pt)
	| otherwise = NOK
reckdtIns (pt) (DeadLeaf lpt)
	| pt == lpt = OK (Leaf pt)
	| otherwise = NOK
reckdtIns pt a@(Node median dim lnode rnode)
	| dim pt < dim median = 
		case reckdtIns pt lnode of
			OK new -> OK (Node median dim new rnode)
			NOK -> NOK
	| dim pt == dim median =
		case reckdtIns pt lnode of
			OK new  -> OK (Node median dim new rnode)
			NOK -> case reckdtIns pt rnode of
				OK new -> OK (Node median dim lnode new)
				NOK -> NOK
	| otherwise =
		case reckdtIns pt rnode of
			OK new -> OK (Node median dim lnode new)
			NOK -> NOK
reckdtIns pt a@(DeadNode median dim lnode rnode)
	| dim pt < dim median = 
		case reckdtIns pt lnode of
			OK new  -> OK (Node median dim new rnode)
			NOK -> NOK
	| dim pt == dim median =
		case reckdtIns pt lnode of
			OK new  -> OK (Node median dim new rnode)
			NOK -> case reckdtIns pt rnode of
				OK new  -> OK (Node median dim lnode new)
				NOK -> NOK
	| otherwise =
		case reckdtIns pt rnode of
			OK new -> OK (Node median dim lnode new)
			NOK -> NOK

