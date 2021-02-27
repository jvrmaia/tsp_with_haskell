{-# OPTIONS_GHC -fglasgow-exts #-}
module Kruskall where
import BTree
import RadixExchange
import Point
import Data.List(delete,union)

type Point' = (Int,Point)
type Aresta = (Point',Dist,Point')
data Adjacencias = Ad Point [Point]

compadj :: Adjacencias -> Adjacencias -> Ordering
compadj (Ad x xs) (Ad y ys)
	|x > y = GT
	|x == y = EQ
        |x < y = LT

kruskall xs = return $ toList $ kruskallAux $ Kruskall.init $ numerapontos (Data.List.union [] xs) 0
	where
		kruskallAux :: ([Radix],BTree.BTree Adjacencias,[Aresta]) -> BTree.BTree Adjacencias
		kruskallAux (mst,adj,stack)
			|null $ tail mst = adj
			|otherwise =
				let (x0,_,x1) = head stack
				    (mst2,adj2)= insertEdge x0 x1 mst adj [] []
				in kruskallAux (mst2,adj2,tail stack)

		numerapontos :: [Point] -> Int -> [Point']
		numerapontos [] _ = []
		numerapontos (x:xs) n = (n,x) : numerapontos xs (n+1)

insertEdge :: Point' -> Point' -> [Radix] -> BTree.BTree Adjacencias -> [Radix] -> [Radix] -> ([Radix],BTree.BTree Adjacencias)
insertEdge x0 x1 (m:mst) adj restantes [] =
	let p0 = membership (fst x0) m
	    p1 = membership (fst x1) m
	in if p0 && p1
	then (m:mst++restantes,adj)
	else if p0 || p1
	then insertEdge x0 x1 mst adj restantes [m]
	else insertEdge x0 x1 mst adj (m:restantes) []
insertEdge x0 x1 (m:mst) adj restantes [aunir] =
	let p0 = membership (fst x0) m
	    p1 = membership (fst x1) m
	in if p0 || p1
	then ((RadixExchange.union m aunir):mst++restantes,Kruskall.union (snd x0) (snd x1) adj)
	else insertEdge x0 x1 mst adj (m:restantes) [aunir]

union :: Point -> Point -> BTree.BTree Adjacencias -> BTree.BTree Adjacencias
union x0 x1 xs =
	case search compadj (Ad x0 []) xs of
		Nothing -> case search compadj (Ad x1 []) xs of
			Nothing -> BTree.insert compadj (Ad x0 [x1]) $ BTree.insert compadj (Ad x1 [x0]) xs
			Just (Ad _ x1s) -> BTree.insert compadj (Ad x0 [x1]) $ BTree.insert compadj (Ad x1 (x0:x1s)) xs
		Just (Ad _ x0s) -> case search compadj (Ad x1 []) xs of
			Nothing -> BTree.insert compadj (Ad x0 (x1:x0s)) $ BTree.insert compadj (Ad x1 [x0]) xs
			Just (Ad _ x1s) -> BTree.insert compadj (Ad x0 (x1:x0s)) $ BTree.insert compadj (Ad x1 (x0:x1s)) xs

init :: [Point'] -> ([Radix],BTree.BTree Adjacencias,[Aresta])
init xs
	|length xs < 2 = error "Few input, try more Points"
	|otherwise = 
		let mst = map (\x -> Node (fst x)) xs
		    adj = BTree.Leaf
		    stack = initStack xs
		in (mst,adj,stack)
	where
		--gera a fila de prioridade com as arestas ordenadas
		initStack :: [Point'] -> [Aresta]
		initStack [] = [] 
		initStack xs = createPQ $ createPQAux xs 
			where
				-- gera a fila de prioridade (ordenada)
				createPQ [] = []
				createPQ (x:xs) = createPQ [a | a<-xs, (snd3 a)<=(snd3 x) ] ++ [x] ++ createPQ  [b | b<-xs, (snd3 b)>(snd3 x) ]
					where snd3 (_,a,_) = a

				-- gera a fila de prioridade auxiliar (desordenada)
				createPQAux [] = []
				createPQAux (x:xs) = (map (makePQ x) xs) ++ (createPQAux xs)
					where makePQ p@(_,p') q@(_,q') = (p,dist p' q',q)

toList :: BTree.BTree Adjacencias -> [Point]
toList adj = 
	case adj of
		BTree.Leaf -> []
		BTree.Node2 _ (Ad pt pts) _ -> fst $ toListLoop pt pts adj
		BTree.Node3 _ (Ad pt pts) _ _ _ -> fst $ toListLoop pt pts adj 
	where
		toListLoop :: Point -> [Point] -> BTree.BTree Adjacencias -> ([Point],BTree.BTree Adjacencias)
		toListLoop pt [] adj = ([pt],adj)
		toListLoop pt (q:qs) adj = let 	Just (Ad prox proxs) = search compadj (Ad q []) adj
						proxs' = Data.List.delete pt proxs
						adj' = BTree.insert compadj (Ad prox proxs') $ BTree.insert compadj (Ad pt qs) adj
						(listq,newadq)= toListLoop prox proxs' adj'
						(listqs,newadqs) =toListLoop pt qs newadq
					in ([pt] ++ listq ++ listqs,newadqs)

