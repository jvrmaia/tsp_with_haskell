module Stack where
import System.Environment

data Stack a =
	Node a (Stack a)
	| Nil
	deriving(Read,Eq,Show)

isEmptyStack :: Stack a -> Bool
isEmptyStack Nil = True
isEmptyStack (Node x _) = False

push :: Ord a => a -> Stack a -> Stack a
push x Nil = Node x Nil
push y stack = Node y stack

pop :: Ord a => Stack a -> Stack a
pop Nil = Nil
pop (Node x next) = next

top :: Stack a -> a
top Nil = error "Empty Stack"
top (Node x next) = x

sizeStack :: Stack a -> Int
sizeStack Nil = 0
sizeStack (Node x next) = 1 + sizeStack next

isInStack :: Ord a => a -> Stack a -> Bool 
isInStack y Nil = False
isInStack y (Node x next)
	| y == x = True
	| otherwise = isInStack y next

stackToList :: Ord a => Stack a -> [a]
stackToList Nil = []
stackToList (Node x next) = x:(stackToList next)

listToStack :: Ord a => [a] -> Stack a
listToStack [] = Nil
listToStack (x:xs) =  Node x (listToStack xs)

