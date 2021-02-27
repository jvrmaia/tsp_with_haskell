{-# OPTIONS_GHC -fglasgow-exts #-}
module Kruskall where
import System.Environment 
import BTree
import RadixExchange
import Point

type Aresta = (Point,Dist,Point)
data Adjacencias = Ad Point [Point]
	deriving (Read,Eq,Show)

compadj :: Adjacencias -> Adjacencias -> Ordering
compadj (Ad x xs) (Ad y ys)
	|x > y = GT
	|x == y = EQ
        |x < y = LT

kruskall xs = return $ kruskallAux $ Kruskall.init xs
	where
		kruskallAux :: ([Radix Point],BTree.BTree Adjacencias,[Aresta]) -> BTree.BTree Adjacencias
		kruskallAux (mst,adj,stack)
			|null $ tail mst = adj
			|otherwise =
				let (x0,_,x1) = head stack
				    ix0 = pointToInteger x0
				    ix1 = pointToInteger x1
				    (mst2,adj2)= insertEdge ix0 x0 ix1 x1 mst adj [] []
				in kruskallAux (mst2,adj2,tail stack)

insertEdge :: Integer -> Point -> Integer -> Point -> [Radix Point] -> BTree.BTree Adjacencias -> [Radix Point] -> [Radix Point] -> ([Radix Point],BTree.BTree Adjacencias)
insertEdge ix0 x0 ix1 x1 (m:mst) adj restantes [] =
	let p0 = membership (==) ix0 x0 m 
	    p1 = membership (==) ix1 x1 m
	in if p0 && p1
	then (m:mst++restantes,adj)
	else if p0 || p1
	then insertEdge ix0 x0 ix1 x1 mst adj restantes [m]
	else insertEdge ix0 x0 ix1 x1 mst adj (m:restantes) []	
insertEdge ix0 x0 ix1 x1 (m:mst) adj restantes [aunir] =
	let p0 = membership (==) ix0 x0 m 
	    p1 = membership (==) ix1 x1 m
	in if p0 || p1
	then ((RadixExchange.union m aunir):mst++restantes,Kruskall.union x0 x1 adj)
	else insertEdge ix0 x0 ix1 x1 mst adj (m:restantes) [aunir]

union :: Point -> Point -> BTree.BTree Adjacencias -> BTree.BTree Adjacencias 
union x0 x1 xs =
	case search compadj (Ad x0 []) xs of
		Nothing -> case search compadj (Ad x1 []) xs of
			Nothing -> BTree.insert compadj (Ad x0 [x1]) $ BTree.insert compadj (Ad x1 [x0]) xs
			Just (Ad _ x1s) -> BTree.insert compadj (Ad x0 [x1]) $ BTree.insert compadj (Ad x1 (x0:x1s)) xs
		Just (Ad _ x0s) -> case search compadj (Ad x1 []) xs of
			Nothing -> BTree.insert compadj (Ad x0 (x1:x0s)) $ BTree.insert compadj (Ad x1 [x0]) xs
			Just (Ad _ x1s) -> BTree.insert compadj (Ad x0 (x1:x0s)) $ BTree.insert compadj (Ad x1 (x0:x1s)) xs

init :: [Point] -> ([Radix Point],BTree.BTree Adjacencias,[Aresta])
init xs
	|length xs < 2 = error "Few input, try more Points" 
	|otherwise = 
		let mst = map (\x -> new (pointToInteger x) x) xs
		    adj = BTree.Leaf
		    stack = initStack xs
		in (mst,adj,stack)
	where
		--gera a fila de prioridade com as arestas ordenadas
		initStack :: [Point] -> [Aresta]
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
					where makePQ p q = (p,dist p q,q)

