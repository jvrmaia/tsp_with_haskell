{-# OPTIONS_GHC -fglasgow-exts #-}
module BinomialHeap where
import System.Environment

type BinomialHeap a = [BinHeap a]

data BinHeap a = B Int a [BinHeap a]
	deriving(Eq,Read,Show)

insert :: (a->a->Ordering) -> a -> BinomialHeap a -> BinomialHeap a
insert comp x [] = [B 0 x []]
insert comp x ys = insertBheap comp (B 0 x []) ys

insertBheap :: (a->a->Ordering) -> BinHeap a -> BinomialHeap a -> BinomialHeap a
insertBheap comp bh [] = [bh]
insertBheap comp (B n x lbh) ((B n2 y lbh2):ys)
	|n<n2 = (B n x lbh):(B n2 y lbh2):ys
	|n==n2 = insertBheap comp (heapMeld comp (B n x lbh) (B n2 y lbh2)) ys
	|otherwise = (B n2 y lbh2):insertBheap comp (B n x lbh) ys
	where
		heapMeld:: (a->a->Ordering) -> BinHeap a -> BinHeap a -> BinHeap a
		heapMeld comp h0@(B n a lbh) h1@(B n2 b lbh2)
			|comp a b == EQ || comp a b == LT = B (n+1) b (h0:lbh2) 
			|otherwise = B (n+1) a (h1:lbh)

bheapDelMax:: (a->a->Ordering) -> BinomialHeap a -> (a,BinomialHeap a)
bheapDelMax comp ((B n a l):lbh) = bheapDelMaxLoop comp (B n a l) lbh []
	where
		bheapDelMaxLoop:: (a->a->Ordering) -> BinHeap a -> BinomialHeap a -> BinomialHeap a -> (a,BinomialHeap a)
		bheapDelMaxLoop comp (B n x lbh) [] xs = (x,merge comp lbh xs)
		bheapDelMaxLoop comp (B n x lbh) ((B n2 y lbh2):ys) xs
			|comp y x == GT = bheapDelMaxLoop comp (B n2 y lbh2) ys (xs++[B n x lbh])
			|otherwise = bheapDelMaxLoop comp (B n x lbh) ys (xs++[B n2 y lbh2])

		merge :: (a->a->Ordering) -> BinomialHeap a -> BinomialHeap a -> BinomialHeap a
		merge comp [] heap = heap
		merge comp (x:xs) heap = merge comp xs $ insertBheap comp x heap

