module RadixExchange where
import Data.Bits

data Radix = 
	Node Int
	|Node2 Radix Int Radix
	deriving (Show)

data InsertionOk = 
	OK Radix
	|NOTOK Radix Int --já carrega com ele o bit a mudar
	
membership :: Int -> Radix -> Bool
membership x (Node y)
	|x==y = True
	|otherwise = False
membership x (Node2 t1 bit t2)
	|testBit x bit = membership x t2
	|otherwise = membership x t1

ins :: Int -> Radix -> Radix
ins x radix = case (recinsert x radix) of
		(OK radix) -> radix
		(NOTOK radix bit) ->
			if testBit x bit
				then Node2 radix bit (Node x)
				else Node2 (Node x) bit radix
	where
		recinsert:: Int -> Radix -> InsertionOk
		recinsert x (Node y)
			|x==y = OK (Node x)
			|otherwise = NOTOK (Node y) (difbit x y)
		recinsert x rad@(Node2 t1 bit t2)
			|testBit x bit = case recinsert x t2 of
				(OK t) -> OK (Node2 t1 bit t)
				(NOTOK t bit2) -> if bit > bit2
						then if testBit x bit2
							then OK (Node2  t1 bit (Node2 t bit2 (Node x)))
							else OK (Node2 t1 bit (Node2 (Node x) bit2 t))
						else NOTOK rad bit2
			|otherwise = case recinsert x t1 of
				(OK t) -> OK (Node2 t bit t2)
				(NOTOK t bit2) -> if bit > bit2 then
							if testBit x bit2
							then OK (Node2 (Node2 t bit2 (Node x)) bit t2)
							else OK (Node2 (Node2 (Node x) bit2 t) bit t2)
						else NOTOK rad bit2

union :: Radix -> Radix -> Radix
union (Node x) t = ins x t
union t (Node y) = ins y t
union n12@(Node2 t1 b12 t2) n34@(Node2 t3 b34 t4)
	|b12 < b34 = union n34 n12
	|otherwise= let	n12' = getmin n12
			n34' = getmin n34
			dif = difbit n12' n34'
		in if dif > b12
		then 	if testBit n34' dif
			then Node2 n12 dif n34
			else Node2 n34 dif n12
		else 	if b12 == b34
			then Node2 (union t3 t1) b12 (union t4 t2)
			else	if dif == b12
				then Node2 t1 b12 (union n34 t2)
				else Node2 (union n34 t1) b12 t2
	where
		getmin (Node x) = x
		getmin (Node2 t _ _) = getmin t

difbit :: Int -> Int -> Int
difbit x y= loop (xor x y) 31
	where
		loop a n
			|testBit a n = n
			|otherwise = loop a (n-1)

