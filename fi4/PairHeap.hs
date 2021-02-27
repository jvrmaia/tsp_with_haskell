{-# OPTIONS_GHC -fglasgow-exts #-}
module PairHeap where
import System.Environment

data PHeap a =
	Empty
	|PHeap a [PHeap a]
	deriving(Eq,Read,Show)

insert:: (a->a->Ordering) -> a -> PHeap a -> PHeap a
insert comp x Empty = (PHeap x [])
insert comp x p@(PHeap a hs) = pheapMeld comp (PHeap x []) p

pheapMeld:: (a->a->Ordering) -> PHeap a -> PHeap a -> PHeap a
pheapMeld comp h0@(PHeap a xs) h1@(PHeap b ys)
	|comp a b == EQ || comp a b == LT = PHeap b (h0:ys)
	|otherwise = PHeap a (h1:xs)

pheapDelMax:: (a->a->Ordering) -> PHeap a -> (a,PHeap a)
pheapDelMax comp (PHeap x []) = (x,Empty)
pheapDelMax comp (PHeap x xs) =
	let
		(h:hs) = ppMeld xs
	in
		(x,foldr (pheapMeld comp) h hs)
	where
		ppMeld xs
			| null xs = xs
			| null (tail xs) = xs
			| (h0:h1:hs) <- xs = (pheapMeld comp h0 h1):(ppMeld hs)

