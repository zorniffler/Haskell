module Zorniffler where


--Implementar una función de orden superior que devuelve el mejor 
--elemento de una lista, donde la condición de ser mejor la mide un 
--número que se asocia a cada elemento de la lista. 
--Por ejemplo, el mejor de la lista ["caminito", "camino", caminante"] 
--sería "caminante" según la longitud, y "caminito" según el número
-- de i's; el mejor de la lista [5,2,9,6,8,3,8,5,7,3] sería 9 si 
--a cada número le asignamos a él mismo (la indentidad), o 2 si a cada 
--número le asignamos su opuesto, u 8 si a cada número le asignamos 
--las veces que lo divide 2.
--Poner ejemplos de llamada para cada ejemplo anterior.


thebest:: (a -> Int) -> [a] -> a
thebest _ [] = error "No existe mejor"
thebest f [x] = x
thebest f (h:h1:t)
	| (f h >= f h1) = thebest f (h:t)
	| otherwise = thebest f (h1:t)

--Funciones para probar.

--thebest length ["cami","camino","caminante"]

numI :: String -> Int
numI s =  numI' s

numI':: [Char] -> Int
numI' [] = 0
numI' (h:t) = if h == 'i' then 1+ numI' t else numI' t


--Implementar la función multifilter, que generaliza filter para una 
--lista de filtros. Aplicarla para obtener 
--los 50 primeros capicua pares no múltiplos de 3 
--([2, 4, 8, 22, 44, 88, 202, ...]).

-- Primera version obteniendo m primeros que cumplan multifiltro.
aplicacion:: Int -> [Int]
aplicacion m = take m (multifilter[capicua,par,nomultidetres] [1..])

-- Version completa pasando funciones y numero de elementos deseados.
aplicacion2 ::(Enum a,Num a) =>Int -> [(a->Bool)] -> [a]
aplicacion2 _ [] = []
aplicacion2 m (f1:fs) = take m (multifilter(f1:fs) [1..])

-- multifilter mediante lo aprendido en clase(empleando funcion auxiliar)
multifilter::[(a -> Bool)] -> [a] -> [a]
multifilter [] _ = []
multifilter _ [] = []
multifilter (f:f1) (h:t)
	| multifilter' (f:f1) h = h: (multifilter (f:f1) t)
	| otherwise = multifilter (f:f1) t


multifilter':: [(a -> Bool)] -> a -> Bool
multifilter' [] _ = True
multifilter' (f:f1) x = f x && multifilter' f1 x


-- Empleando herramientas como pliegues.
multifilter2:: [(a -> Bool)] -> [a] -> [a]
multifilter2 fs xs = foldl(\acc f -> filter f acc) xs fs

-- O por la derecha
multifilter3:: [(a -> Bool)] -> [a] -> [a]
multifilter3 fs xs = foldr filter xs fs


-- Funciones para hacer pruebas.
capicua:: Int -> Bool
capicua n = (r == reverse r)
	where r = intToList n


intToList:: Int -> [Int]
intToList n
	| n < 10 = [n]
	| otherwise = (mod n 10):(intToList (div n 10))

par :: Int -> Bool
par n = if mod n 2 == 0 then True else False


nomultidetres:: Int -> Bool
nomultidetres n = if mod n 3 /= 0 then True else False
