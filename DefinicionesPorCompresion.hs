module HaskellMedio where

--Suma de los cuadrados de los n primeros números
sumaDeCuadrados :: Integer -> Integer
sumaDeCuadrados n = sum[x^2 | x <- [1..n]]

--Listas con un elemento replicado
replica :: Int -> a -> [a]
replica n a = [a | _ <- [1..n]]

--Triangulos aritmeticos
--Definir la funcion suma tal que(suma n) es la suma de los n primeros numeros.
suma:: Int -> Int
suma n = sum[x | x<-[1..n]]

--Linea enesima de los triangulos aritmeticos
linea:: Int -> [Int]
linea n = [suma (n-1)+1..suma n]

--Definir la funcion triangulo tal que (triangulo n) es el triangulo aritmetico de altura n.
triangulo:: Int -> [[Int]]
triangulo n = [linea m | m<-[1..n]]

--Numeros perfectos
--Un numero entero es perfecto si es igual a la suma de sus factores
perfectos :: Int -> [Int]
perfectos n = [x | x <- [1..n],sum(init(factoriales x))==x]

factoriales:: Int -> [Int]
factoriales n = [x | x <- [1..n], mod n x ==0]


--Numeros abundantes
numeroAbundante:: Int -> Bool
numeroAbundante n = n < sum (divisores n) 

divisores:: Int -> [Int]
divisores n = [x | x <-[1..n-1], mod n x==0]


--Numeros abundantes menores
numerosAbundantesMenores :: Int -> [Int]
numerosAbundantesMenores n = [x | x <-[1..n],numeroAbundante x]

--Verifica si todos los numeros abundantes menores son pares.
todoPares:: Int -> Bool
todoPares n =  and [even x | x<- numerosAbundantesMenores n]

--Primer numero abundante impar
primerAbundanteImpar :: Int
primerAbundanteImpar = cabeza [x | x<-[1..],numeroAbundante x, odd x]

cabeza:: [a] -> a
cabeza (h:_) = h

--Suma de todos los multiplos de 3 o 5 menores que n.
euler1::Integer -> Integer
euler1 n =  sum [x | x <- [1..n-1],multiplo  x 3 || multiplo  x 5]

multiplo:: Integer -> Integer -> Bool
multiplo a b = mod a b == 0

--Definir la funcion aproxE es la lista cuyos elementos son terminos de la sucesion (1+1/m)^m desde 1 hasta n 
aproxE :: Float -> [Float]
aproxE n = [(1+1/x)**x | x <- [1..n]]

errorAproxE :: Float -> Float
errorAproxE x = head [m | m <- [1..], abs((exp 1)-(1+1/m)**m) < x]

--Definir la constante e 
e = 2.71828459

--Aproximacion del limite
aproxLimSeno:: Float -> [Float]
aproxLimSeno n = [sin(1/m)/(1/m)| m <- [1..n]]

--Menor numero de terminos de la sucesion sin(1/m)/(1/m) necesarios para obtener su limite con un error menor que x
errorLimSeno:: Float -> Float
errorLimSeno x = head [m | m <- [1..], abs(1 - sin(1/m)/(1/m)) < x ]

--Calculo del número pi
calculaPi:: Float -> Float
calculaPi n = 4* sum [(-1)**m/(2*m+1) | m <- [0..n]]

errorPi:: Float -> Float
errorPi n = head [ m | m <- [1..], abs(pi - (calculaPi m)) < n]

--Ternas pitagóricas
pitagoricas :: Int -> [(Int,Int,Int)]
pitagoricas n = [(x,y,z) | x <- [1..n],
						   y <- [1..n],
						   z <- [1..n],
						   x^2 + y^2 ==z^2]
						   
--Numero de elementos pares de la terna t
numeroDePares:: (Int,Int,Int) -> Int
numeroDePares (x,y,z) = sum[1 | n <- [x,y,z], even n]

--Conjetura verifica si todas las ternas pitagoricas entre 1 y n tiene un numero impar de numeros pares.
conjetura :: Int -> Bool
conjetura n = and [odd(numeroDePares t) | t <- pitagoricas n] 

--Lista de ternas pitagoricas que sumen x
ternasPitagoricas:: Int -> [(Int,Int,Int)]
ternasPitagoricas x = [(a,b,c) | a <- [1..x], b <- [a+1..x], c <- [x-a-b], a^2 + b^2 == c^2]

--Produco escalar de dos listas de enteros xs e ys
productoEscalar :: [Int] -> [Int] -> Int
productoEscalar xs ys = sum [x*y | (x,y) <- zip xs ys]

-- Representacion densa de un polinomio representado dispersamente
densa:: [Int] -> [(Int,Int)]
densa xs = [(x,y) | (x,y) <- zip [n-1,n-2..0] xs, y /= 0]
	where n =length xs

--Consulta de bases de datos
personas:: [(String,String,Int,Int)]
personas = [("Cervantes","Literatura",1547,1616),
			("Velazquez","Pintura",1599,1660),
			("Picasso","Pintura",1881,1973)]

--Definir la funcion nombres que es la lista de los nombres de la base de datos bd
nombres :: [(String,String,Int,Int)] -> [String]
nombres bd = [x | (x,_,_,_) <- bd]



