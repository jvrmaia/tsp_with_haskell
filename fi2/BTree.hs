module BTree where

import Data.List(sort,nub,unfoldr,(\\),intersect)

data BTree a =  
	Leaf
	|Node2 (BTree a) a (BTree a)
	|Node3 (BTree a) a (BTree a) a (BTree a)
	deriving (Eq,Read,Show)

search :: (a -> a ->Ordering) -> a -> BTree a -> Maybe a
search _ _ Leaf = Nothing
search ord x (Node2 l a r) =
	case ord x a of
		LT -> search ord x l
		EQ -> (Just a)
		GT -> search ord x r
search ord x (Node3 l a m b r) =
	case ord x a of
		LT -> search ord x l
		EQ -> (Just a)
		GT -> case ord x b of
			LT -> search ord x m
			EQ -> (Just b)
			GT -> search ord x r

data TreeSplit a =
	Split (BTree a)
	| NoSplit (BTree a)
	deriving (Eq,Read,Show)

insert :: (a -> a ->Ordering) -> a -> BTree a -> BTree a
insert ord x t = case (recinsert ord x t) of
		(Split t) -> t
		(NoSplit t) -> t

recinsert :: (a -> a ->Ordering) -> a -> BTree a -> TreeSplit a
recinsert ord x Leaf = Split (Node2 Leaf x Leaf)
recinsert ord x (Node2 l a r) =
	case ord x a of
		LT -> case recinsert ord x l of
			(Split (Node2 ln n rn)) -> NoSplit (Node3 ln n rn a r)
			(NoSplit newl) -> NoSplit (Node2 newl a r)
		EQ -> NoSplit (Node2 l x r)
		GT -> case recinsert ord x r of
			(Split (Node2 ln n rn)) -> NoSplit (Node3 l a ln n rn)
			(NoSplit newr) -> NoSplit (Node2 l a newr)
recinsert ord x (Node3 l a m b r) =
	case ord x a of
		LT -> case recinsert ord x l of
			(Split newl) -> Split (Node2 newl a (Node2 m b r))
			(NoSplit newl) -> NoSplit (Node3 newl a m b r)
		EQ -> NoSplit (Node3 l x m b r)
		GT -> case ord x b of
			LT -> case recinsert ord x m of
				(Split (Node2 lm up rm)) -> Split (Node2 newl up newr)
					where newl = Node2 l a lm
					      newr = Node2 rm b r
				(NoSplit newm) -> NoSplit (Node3 l a newm b r)
			EQ -> NoSplit (Node3 l a m x r)
			GT -> case recinsert ord x r of
				(Split newr) -> Split (Node2 newl b newr)
					where newl = Node2 l a m
				(NoSplit newr) -> NoSplit (Node3 l a m b newr)

-- funcao a ser aplicada em casa elemento, funcao que acumula, b0 
accFInBTree :: (a -> b) -> (b -> b -> b) -> b -> BTree a -> b
accFInBTree f acc b0 Leaf = b0
accFInBTree f acc b0 (Node2 l a r) = acc (accFInBTree f acc b0 l) $ acc (f a) (accFInBTree f acc b0 r) 
accFInBTree f acc b0 (Node3 l a m b r) = acc (accFInBTree f acc b0 l) $ acc (accFInBTree f acc b0 m) $ acc (accFInBTree f acc b0 r) $ acc (f a) (f b)

