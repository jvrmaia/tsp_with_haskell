{-# OPTIONS_GHC -fglasgow-exts #-}
module Prototipo where
import System.Environment 
import Data.List
import ListaPrioridade
import Tour
import Point

--Inicia um tour através da maior distância entre 2 pontos na lista	
init :: [Point] -> (Tour,ListaPrioridade)
init xs
	| length xs < 2 = error "Few input, try more Points" 
	| otherwise = (initTour x0 x1 xs, initPointsNotInTour x0 x1 xs)
	where
		(x0,x1) = pointsMostFar (head xs) (head $ tail xs) (tail $ tail xs)
		pointsMostFar :: Point -> Point -> [Point] -> (Point,Point)  
		pointsMostFar x0 x1 [] = (x0,x1)
		pointsMostFar x0 x1 (x:xs)
			|dist x x1 > dist x1 x0 && dist x x1 > dist x x0 = pointsMostFar x1 x xs
			|dist x x0 > dist x1 x0 = pointsMostFar x0 x xs
			|otherwise = pointsMostFar x0 x1 xs

--Calcula o melhor tour a partir de uma lista de pontos utilizando a heurística Furthest Insertion
tsp :: (Monad m) => [Point] -> m [Point]
tsp xs = return $ toList $ tspAux $ Prototipo.init (union [] xs)
	where
		
	tspAux :: (Tour,ListaPrioridade) -> Tour
	tspAux (tour,priori)
		|ListaPrioridade.null priori = tour
		|x1 == pointMostFar = tspAux (insertPointInTour (x0,x1) tour,xs')
		|otherwise = tspAux (tour,novopriori)
		where
			((x0,x1),xs') = selectMostFar priori
			pointMostFar = nn x0 tour
			novopriori = ListaPrioridade.insert x0 pointMostFar xs'

