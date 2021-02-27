{-# OPTIONS_GHC -fglasgow-exts #-}
module Dijkstra where
import System.Environment 
import KDTree3
import BTree3
import BTree
import RadixExchange
import Point
import Data.List(delete,union)

type Point' = (Int,Point)

type Aresta = (Point',Dist,Point')
data Adjacencias = Ad Point [Point]
	deriving (Read,Eq,Show)

compadj :: Adjacencias -> Adjacencias -> Ordering
compadj (Ad x xs) (Ad y ys)
	|x > y = GT
	|x == y = EQ
        |x < y = LT

comparesta :: Aresta -> Aresta -> Ordering
comparesta (_,a,_) (_,b,_)
	|a > b = GT
	|a == b = EQ
        |a < b = LT

topoint :: Point' -> Point = snd

dijkstra xs = return $ toList $ dijkstraAux $ Dijkstra.init $ numerapontos (Data.List.union [] xs) 0
	where
		dijkstraAux :: (Radix,BTree.BTree Adjacencias,BTree3.BTree Aresta,KDTree Point') -> BTree.BTree Adjacencias
		dijkstraAux (mst,adj,priori,kdt)
			|vazio kdt = adj
			|otherwise =
				let ((x0,_,x1),priori') = deleteMin priori
				    near0 = kdtNN topoint x0 kdt
				    near1 = kdtNN topoint x1 kdt
				in if membership (fst x0) mst
				then dijkstraAux (mst,adj,BTree3.insert comparesta (near1,dist (topoint near1) (topoint x1),x1) $ priori',kdt)
				else dijkstraAux (ins (fst x0) mst,Dijkstra.union (snd x0) (snd x1) adj,BTree3.insert comparesta (near0,dist (topoint near0) (topoint x0),x0) $ BTree3.insert comparesta (near1,dist (topoint near1) (topoint x1),x1) priori',kdtDel topoint x0 kdt)
		numerapontos :: [Point] -> Int -> [Point']
		numerapontos [] _ = []
		numerapontos (x:xs) n = (n,x) : numerapontos xs (n+1)

init :: [Point'] -> (Radix,BTree.BTree Adjacencias,BTree3.BTree Aresta,KDTree Point')
init xs
	|length xs < 2 = error "Few input, try more Points" 
	|otherwise = 
		let ((id0,x0),(id1,x1)) = pointsMostNear (head xs) (head $ tail xs) (tail $ tail xs)
		    mst = ins id1 (RadixExchange.Node id0)
		    adj = Dijkstra.union x1 x0 BTree.Leaf
		    kdt = kdtDel topoint (id0,x0) $ kdtDel topoint (id1,x1) $ kdtBuild topoint xs
		    near0 = kdtNN topoint (id0,x0) kdt
		    near1 = kdtNN topoint (id1,x1) kdt
		    priori = BTree3.insert comparesta (near0,dist (topoint near0) x0,(id0,x0)) $ BTree3.insert comparesta (near1,dist (topoint near1) x1,(id1,x1)) BTree3.Leaf
		in (mst,adj,priori,kdt)
	where
		pointsMostNear :: Point' -> Point' -> [Point'] -> (Point',Point')  
		pointsMostNear (id0,x0) (id1,x1) [] = ((id0,x0),(id1,x1))
		pointsMostNear (id0,x0) (id1,x1) ((id,x):xs)
			|dist x x1 < dist x1 x0 && dist x x1 < dist x x0 = pointsMostNear (id,x) (id1,x1) xs
			|dist x x0 < dist x1 x0 = pointsMostNear (id0,x0) (id,x) xs
			|otherwise = pointsMostNear (id0,x0) (id1,x1) xs

union :: Point -> Point -> BTree.BTree Adjacencias -> BTree.BTree Adjacencias 
union x0 x1 xs =
	case search compadj (Ad x0 []) xs of
		Nothing -> case search compadj (Ad x1 []) xs of
			Nothing -> BTree.insert compadj (Ad x0 [x1]) $ BTree.insert compadj (Ad x1 [x0]) xs
			Just (Ad _ x1s) -> BTree.insert compadj (Ad x0 [x1]) $ BTree.insert compadj (Ad x1 (x0:x1s)) xs
		Just (Ad _ x0s) -> case search compadj (Ad x1 []) xs of
			Nothing -> BTree.insert compadj (Ad x0 (x1:x0s)) $ BTree.insert compadj (Ad x1 [x0]) xs
			Just (Ad _ x1s) -> BTree.insert compadj (Ad x0 (x1:x0s)) $ BTree.insert compadj (Ad x1 (x0:x1s)) xs

toList :: BTree.BTree Adjacencias -> [Point]
toList adj = 
	case adj of
		BTree.Leaf -> []
		BTree.Node2 _ (Ad pt pts) _ -> fst $ toListLoop pt pts adj
		BTree.Node3 _ (Ad pt pts) _ _ _ -> fst $ toListLoop pt pts adj 
	where
		toListLoop :: Point -> [Point] -> BTree.BTree Adjacencias -> ([Point],BTree.BTree Adjacencias)
		toListLoop pt [] adj = ([pt],adj)
		toListLoop pt (q:qs) adj = let Just (Ad prox proxs) = search compadj (Ad q []) adj
					       proxs' = Data.List.delete pt proxs
					       adj' = BTree.insert compadj (Ad prox proxs') $ BTree.insert compadj (Ad pt qs) adj
					       (listq,newadq)= toListLoop prox proxs' adj'
					       (listqs,newadqs) =toListLoop pt qs newadq
					in ([pt] ++ listq ++ listqs,newadqs)

