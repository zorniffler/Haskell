module Entregable where 


-- ALEJANDRO ZORNOZA MARTÍNEZ
-- Programación Declarativa 2016/2017.

--    a) Implementar una función que comprueba si un entero positivo
-- tiene exactamente k veces un dígito d.

	
-- Paso de un entero a una lista.
intToList :: Int -> [Int]
intToList n 
 | n < 10 = [n]
 | otherwise = intToList (div n 10) ++ [mod n 10]


-- Paso a subfuncion con el paso a lista.
vecesD:: Int -> Int -> Int -> Bool
vecesD n k d = vecesD' (intToList n) k d


-- Función para contar apariciones.
vecesD':: [Int] -> Int -> Int -> Bool
vecesD' (h:t) k d
	| h == d = vecesD' t (k-1) d
	| otherwise = vecesD' t k d
vecesD' []  k d
	| k == 0 = True
	| otherwise = False



--b) Implementar una función que obtiene una lista con los m primeros enteros 
--que cumplen la propiedad anterior, para k y d dados
--Por ejemplo, la lista con los 8 primeros enteros que contienen 3 1's es [1115,1114,1113,1112,1110,1101,1011,111] (en este u otro orden).


-- Aplicación de listas intensionales (No del temario tema 5).
listP:: Int -> Int -> Int -> [Int]
listP m k d = take m [n | n <- [1..], vecesD n k d ]


-- Aplicación con temario dado.
listPsencillo:: Int -> Int -> Int -> [Int]
listPsencillo m k d = listPsencillo' 1 m k d


listPsencillo':: Int -> Int -> Int -> Int -> [Int]
listPsencillo' _ 0 _ _ = []
listPsencillo' n m k d
		| vecesD n k d = (n:(listPsencillo' (n+1) (m-1) k d))
		| otherwise = (listPsencillo' (n+1) m k d)



--Implementar una función que simula el movimiento de las torres de Hanoi, devolviendo una lista de movimientos. Suponemos los postes declarados como

--Por ejemplo, la llamada hanoi 4 A B C significa pasar 4 círculos del poste A al B usando C como auxiliar, y devolvería
-- [(A,C),(A,B),(C,B),(A,C),(B,A),(B,C),(A,C),(A,B),(C,B),(C,A),(B,A),(C,B),(A,C),(A,B),(C,B)] 
--que indica la secuencia de movimientos del poste A al C, del A al B, del C al B, etc. 

--Ayuda: Para pasar n círculos de x a y usando z como auxiliar (e.d., hanoi n x y z), pasar n-1 de x a z usando y, 1 de x a y, 
--y después (n-1) de z a y usando x



-- Definición TAD
data Poste = A | B | C deriving Show


-- Función hanoi.
pasarPoste:: Int -> Poste -> Poste -> Poste -> [(Poste,Poste)]
pasarPoste 1 x y z = [(x,y)]
pasarPoste n x y z = pasarPoste (n-1) x z y ++ pasarPoste 1 x y z ++ pasarPoste (n-1) z y x


