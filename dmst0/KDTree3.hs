{-# OPTIONS_GHC -fglasgow-exts #-}
module KDTree3 where
import Data.List
import Point

type Dimen = Point -> Double
data KDTree a =
	Leaf a
	|DeadLeaf a
	|Node Point Dimen (KDTree a) (KDTree a)
	|DeadNode Point Dimen (KDTree a) (KDTree a)

data DelOK a=
	OK (KDTree a)
	|NOK

kdtBuild :: (a -> Point) -> [a]-> KDTree a
kdtBuild topoint [pt] = Leaf (pt)
kdtBuild topoint [pt0,pt1] -- como default p/ 2 nums sempre por y!!!
	|ycoord (topoint pt0) <= ycoord (topoint pt1) = Node (topoint pt0) ycoord (Leaf pt0) (Leaf pt1)
	|otherwise = Node (topoint pt1) ycoord (Leaf pt1) (Leaf pt0)
kdtBuild topoint (p0:p1:p2:pts) = let 	pt0 = topoint p0
					pt1 = topoint p1
					pt2 = topoint p2

					ld
						|(xmax-xmin)<(ymax-ymin) = ycoord
						|otherwise = xcoord
		
					((xmin,ymin),(xmax,ymax)) = foldr (xyminmax topoint) (topoint p0,topoint p0) (p1:p2:pts)
					xyminmax topoint pt ((xmi,ymi),(xma,yma)) = let (xp,yp) = topoint pt
						in ((min xmi xp,min ymi yp),(max xma xp,max yma yp))

					(mi,md,ma)
						|ld pt0<=ld pt1 && ld pt1<=ld pt2 = (p0,p1,p2)
						|ld pt0<=ld pt2 && ld pt2<=ld pt1 = (p0,p2,p1)
						|ld pt1<=ld pt0 && ld pt0<=ld pt2 = (p1,p0,p2)
						|ld pt1<=ld pt2 && ld pt2<=ld pt0 = (p1,p2,p0)
						|ld pt2<=ld pt0 && ld pt0<=ld pt1 = (p2,p0,p1)
						|ld pt2<=ld pt1 && ld pt1<=ld pt0 = (p2,p1,p0)
	in Node (topoint md) ld (kdtBuild topoint $ mi:md:[x|x<-(pts),ld (topoint x) <= ld (topoint md)]) (kdtBuild topoint $ ma:[x|x<-(pts),ld (topoint x)>ld (topoint md)])

kdtNN :: (a -> Point) -> a -> KDTree a -> a
kdtNN topoint a (Leaf b) = b
kdtNN topoint a (Node median dim lnode rnode)
	| vazio lnode = kdtNN topoint a rnode
	| vazio rnode = kdtNN topoint a lnode
	|otherwise= let	pt = topoint a
			nnl = kdtNN topoint a lnode
			dl = dist (topoint nnl) pt
			nnr = kdtNN topoint a rnode
			dr = dist (topoint nnr) pt
			in if dim pt <= dim median 
			then	if dim pt + dl > dim median
				then if dr < dl
					then nnr
					else nnl
				else nnl
			else 	if dim pt - dr < dim median
				then if dl < dr
					then nnl
					else nnr
				else nnr

kdtDel :: (a -> Point) -> a -> KDTree a -> KDTree a
kdtDel topoint a tree =
	case reckdtDel topoint a tree of
		OK t -> t
		NOK -> tree

reckdtDel :: (a -> Point) -> a -> KDTree a -> DelOK a
reckdtDel topoint b (Leaf lpt)
	| topoint b == topoint lpt = OK (DeadLeaf b)
	| otherwise = NOK
reckdtDel topoint _ (DeadLeaf lpt) = NOK
reckdtDel topoint _ a@(DeadNode median dim lnode rnode) = NOK
reckdtDel topoint b a@(Node median dim lnode rnode) = let pt = topoint b 
	in if dim pt < dim median 
	then	case reckdtDel topoint b lnode of
			OK new -> if vazio new && vazio rnode
				then OK (DeadNode median dim new rnode)
				else OK (Node median dim new rnode)
			NOK -> NOK
	else if dim pt == dim median
	then	case reckdtDel topoint b lnode of
			OK new  -> if vazio new && vazio rnode
				then OK (DeadNode median dim new rnode)
				else OK (Node median dim new rnode)
			NOK -> case reckdtDel topoint b rnode of
				OK new -> if vazio new && vazio lnode
					then OK (DeadNode median dim lnode new)
					else OK (Node median dim lnode new)
				NOK -> NOK
	else	case reckdtDel topoint b rnode of
			OK new -> if vazio new && vazio lnode
				then OK (DeadNode median dim lnode new)
				else OK (Node median dim lnode new)
			NOK -> NOK

vazio tree = case tree of
	DeadLeaf _ -> True
	DeadNode _ _ _ _ -> True
	Leaf _ -> False
	Node _ _ _ _ -> False

