{-# OPTIONS_GHC -fglasgow-exts #-}
module Dijkstra where
import System.Environment 
import KDTree3
import BTree3
import RadixExchange
import Point
import Data.List(union)

data Mst =
	Empty
	|Mst Point [Mst] Radix

comp :: (Point',Dist,Point') -> (Point',Dist,Point') -> Ordering
comp (_,a,_) (_,b,_)
	|a > b = GT
	|a == b = EQ
        |a < b = LT

type Point' = (Int,Point)
topoint :: Point' -> Point = snd

dijkstra xs = return $ dijkstraAux $ Dijkstra.init  $ numerapontos (Data.List.union [] xs) 0
	where
		dijkstraAux :: (Mst,BTree (Point',Dist,Point'),KDTree Point') -> Mst
		dijkstraAux (mst,priori,kdt)
			|vazio kdt = mst
			|otherwise=let 	(Mst _ _ radix) = mst
					(((ix0,x0),_,(ix1,x1)),priori') = deleteMin priori
					(in0,near0) = kdtNN topoint (ix0,x0) kdt
		    			(in1,near1) = kdtNN topoint (ix1,x1) kdt
				in if membership ix0 radix -- neste caso x0 era o + prox d + d um pto do tour, temos q atualizar isso!!!
				then dijkstraAux (mst,insert comp ((in1,near1),dist near1 x1,(ix1,x1)) $ priori',kdt)
				else dijkstraAux (insertMst (ix0,x0) (ix1,x1) mst,insert comp ((in0,near0),dist near0 x0,(ix0,x0)) $ insert comp ((in1,near1),dist near1 x1,(ix1,x1)) priori',kdtDel topoint (ix0,x0) kdt)
		numerapontos :: [Point] -> Int -> [Point']
		numerapontos [] _ = []
		numerapontos (x:xs) n = (n,x) : numerapontos xs (n+1)

init :: [Point'] -> (Mst,BTree (Point',Dist,Point'),KDTree Point')
init xs
	|length xs < 2 = error "Few input, try more Points" 
	|otherwise = 
		let ((ix0,x0),(ix1,x1)) = pointsMostNear (head xs) (head $ tail xs) (tail $ tail xs)
		    mstinicial = Mst x1 [Mst x0 [] (RadixExchange.Node ix0)] (ins ix1 (RadixExchange.Node ix0))
		    kdt = kdtDel topoint (ix0,x0) $ kdtDel topoint (ix1,x1) $ kdtBuild topoint xs
		    (in0,near0) = kdtNN topoint (ix0,x0) kdt
		    (in1,near1) = kdtNN topoint (ix1,x1) kdt
		    priori = insert comp ((in0,near0),dist near0 x0,(ix0,x0)) $ insert comp ((in1,near1),dist near1 x1,(ix1,x1)) BTree3.Leaf
		in (mstinicial,priori,kdt)
	where
		pointsMostNear :: Point' -> Point' -> [Point'] -> (Point',Point')  
		pointsMostNear (id0,x0) (id1,x1) [] = ((id0,x0),(id1,x1))
		pointsMostNear (id0,x0) (id1,x1) ((id,x):xs)
			|dist x x1 < dist x1 x0 && dist x x1 < dist x x0 = pointsMostNear (id,x) (id1,x1) xs
			|dist x x0 < dist x1 x0 = pointsMostNear (id0,x0) (id,x) xs
			|otherwise = pointsMostNear (id0,x0) (id1,x1) xs

insertMst :: Point' -> Point' -> Mst -> Mst
insertMst (idy,y) _ Empty = Mst y [] (RadixExchange.Node idy)
insertMst (idy,y) _ (Mst x [] radix) = Mst x [Mst y [] (RadixExchange.Node idy)] (ins idy radix)
insertMst (idy,y) (idn,near) (Mst x adj@(q:qs) radix) 
	| x == near = Mst x ((Mst y [] (RadixExchange.Node idy)):adj) (ins idy radix)
	| otherwise = Mst x (insertMst'(idy,y) (idn,near) (q:qs)) (ins idy radix)
	where
		insertMst' (idy,y) (idn,near) [] = []
		insertMst' (idy,y) (idn,near) ((Mst pt pts radix):qs)
			|membership idn radix = (insertMst (idy,y) (idn,near) (Mst pt pts radix)):qs
			|otherwise = (Mst pt pts radix):insertMst' (idy,y) (idn,near) qs
