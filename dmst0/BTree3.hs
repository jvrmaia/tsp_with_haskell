module BTree3 where

data BTree a =  
	Leaf
	|Node2 (BTree a) a (BTree a)
	|Node3 (BTree a) a (BTree a) a (BTree a)
	deriving (Eq,Read,Show)

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
		GT -> case recinsert ord x l of
			(Split (Node2 ln n rn)) -> NoSplit (Node3 ln n rn a r)
			(NoSplit newl) -> NoSplit (Node2 newl a r)
		EQ -> case recinsert ord x l of
			(Split (Node2 ln n rn)) -> NoSplit (Node3 ln n rn a r)
			(NoSplit newl) -> NoSplit (Node2 newl a r)
		LT -> case recinsert ord x r of
			(Split (Node2 ln n rn)) -> NoSplit (Node3 l a ln n rn)
			(NoSplit newr) -> NoSplit (Node2 l a newr)
recinsert ord x (Node3 l a m b r) =
	case ord x a of
		GT -> case recinsert ord x l of
			(Split newl) -> Split (Node2 newl a (Node2 m b r))
			(NoSplit newl) -> NoSplit (Node3 newl a m b r)
		EQ -> case recinsert ord x l of
			(Split newl) -> Split (Node2 newl a (Node2 m b r))
			(NoSplit newl) -> NoSplit (Node3 newl a m b r)
		LT -> case ord x b of
			GT -> case recinsert ord x m of
				(Split (Node2 lm up rm)) -> Split (Node2 newl up newr)
					where newl = Node2 l a lm
					      newr = Node2 rm b r
				(NoSplit newm) -> NoSplit (Node3 l a newm b r)
			EQ -> case recinsert ord x m of
				(Split (Node2 lm up rm)) -> Split (Node2 newl up newr)
					where newl = Node2 l a lm
					      newr = Node2 rm b r
				(NoSplit newm) -> NoSplit (Node3 l a newm b r)
			LT -> case recinsert ord x r of
				(Split newr) -> Split (Node2 newl b newr)
					where newl = Node2 l a m
				(NoSplit newr) -> NoSplit (Node3 l a m b newr)

data TreeHole a = Hole a (BTree a)
		| NoHole a (BTree a)

deleteMin :: BTree a -> (a,BTree a)
deleteMin t = case (recdeleteMin t) of
		(Hole a t) -> (a,t)
		(NoHole a t) -> (a,t)

recdeleteMin :: BTree a -> TreeHole a
recdeleteMin t@(Node2 Leaf a Leaf) = Hole a Leaf
recdeleteMin t@(Node3 Leaf a Leaf b Leaf) = NoHole b (Node2 Leaf a Leaf)
recdeleteMin t@(Node2 l a r) = 
	case (recdeleteMin r) of
		(Hole min down) -> rotaciona min l a down
		(NoHole min newr) -> NoHole min (Node2 l a newr)
	where
		rotaciona min (Node2 ll al rl) a down = Hole min (Node3 ll al rl a down)
		rotaciona min (Node3 ll al ml bl rl) a down= NoHole min (Node2 (Node2 ll al ml) bl (Node2 rl a down)) 
recdeleteMin t@(Node3 l a m b r) = 
	case (recdeleteMin r) of
		(Hole min down) -> rotaciona min l a m b down
		(NoHole min newr) -> NoHole min (Node3 l a m b newr)  
	where
		rotaciona min l a (Node2 lm am rm) b down = NoHole min (Node2 l a (Node3 lm am rm b down))
		rotaciona min l a (Node3 lm am mm bm rm) b down = NoHole min (Node3 l a (Node2 lm am mm) bm (Node2 rm b down))

