module HaskellBasico where

--Obtener la media de 3 numeros
media3 x y z = (x+y+z)/3

--Definir la funcion sumaMonedas tal(sumaMonedas a b c d e) es la suma
--de los euros corespondientes a a monedas de 1 euro,b de 2 euros, c de 5 euros, d 10 euros y e de 20 euros.
sumaMonedas:: Int -> Int ->Int ->Int ->Int -> Int
sumaMonedas a b c d e = 1*a+2*b+5*c+10*d+20*e

--Definir la funcion volumen de una esfera(volumenEsfera r)
volumenEsfera:: Float -> Float
volumenEsfera r = (4/3)*pi*r^3

--Definir la funcion areaDeCoronaCircular tal que (areaDeCoronaCircular r1 r2)
areaDeCoronaCircular:: Float -> Float -> Float
areaDeCoronaCircular r1 r2 = pi*(r2^2-r1^2)

--Ultima cifra de un numero entero(ultimaCifra x)
ultimaCifra:: Int -> Int
ultimaCifra x = rem' x 10

rem'::Int -> Int -> Int
rem' x y = mod x y

--Maximo de 3 elementos (maxTres x y z)
maxTres:: Int -> Int ->Int ->Int
maxTres a b c = (max' (max' a b) c)

max':: Int -> Int -> Int
max' a b = if a > b then a
			else (
				if a<b then b
				else a)
			
--Calculo de disyuncion excluyente usando la tabla de verdad
xor1:: Bool -> Bool -> Bool
xor1 True True = False
xor1 True _ = True
xor1 _ True = True
xor1 _ _ = False			
			
--Calculo de disyuncion excluyente usando 2 ecuaciones
xor2:: Bool -> Bool ->Bool			
xor2 True x = not' x
xor2 False x = x

not'::Bool -> Bool
not' False = True
not' _ = False			
			
--Define la funcion rota1 tal que (rota1 xs) ponga el primer elemento de xs el ultimo
rota1:: [a] -> [a]
rota1 [] = []
rota1 (h:t) = t++[h]			
			
--Define la funcion rota tal que (rota n xs) es la lista obtenida rotando los n primeros elementos de xs al final de la lista			
rota:: Int -> [a] -> [a]
rota 0 xs = xs
rota n (h:t)= (rota (n-1) t)++[h]			

--Define la funcion rango de la lista (rango xs) que es una lista formada por el menor y el mayor elemento de xs			
rango:: (Ord a)=>[a] -> [a]
rango [] = []
rango xs = [minL' xs, maxL' xs]

minL'::(Ord a) => [a] -> a
minL' [] = error "Lista vacia"
minL' [h] = h
minL' (h:t)
		| h<rec = h
		| otherwise = rec
			where rec = minL' t
			
maxL'::(Ord a) => [a] -> a
maxL' [] = error "Lista vacia"
maxL' [h] = h
maxL' (h:t)
		| h>rec = h
		| otherwise = rec
			where rec = maxL' t			
			
--Palindromos			
palindromo::(Eq a)=> [a] -> Bool
palindromo xs = xs == reverse' xs			

reverse':: [a] -> [a]
reverse'[h] = [h]
reverse' (h:t)= (reverse' t)++[h]
	

--Definir la funcion interior tal que se eliminen los extremos de una lista
interior:: [a] -> [a]
interior  [] = []
interior xs = tail(init' xs)

tail':: [a] -> [a]
tail' [] = []
tail' (h:t)	= t

init' :: [a] -> [a]
init' [] = []
init' [x] = []
init' (h:t) = h:(init' t)

--Definir la funcion finales tal que (finales n xs) es la lista formada por los n finales elementos de xs
finales:: Int -> [a] -> [a]
finales n xs = drop(length xs - n) xs



--Segmentos de una lista(segmento m n xs) es la lista de los elementos de xs comprendidos entre y m y n
segmento:: Int -> Int -> [a] -> [a]
segmento _ _ [] = []
segmento _ _ [x] = [x]
segmento m n xs= drop (m-1) (take n xs)

--Igualdad de 3 elementos
tresIguales:: Int -> Int ->Int -> Bool
tresIguales a b c = a==b && b==c

--Igualdad de 4 elementos
cuatroIguales:: Int -> Int -> Int -> Int -> Bool
cuatroIguales  a b c d = (tresIguales a b c) && c==d


--Propiedad triangular(la longitud de cada lado debe ser menor que la suma de los otros dos lados)
triangular:: Int -> Int ->Int -> Bool
triangular a b c = a < b+c && b < a+c && c < a+b

--Division segura( si es por 0 devolver error)
divisionSegura:: Float -> Float -> Float
divisionSegura a b
	| b==0 = error "No se puede division por cero"
	| otherwise = a/b

divisionSegura':: Float -> Float -> Float
divisionSegura' _ 0 = error "No se puede dividir por cero"
divisionSegura' a b = a/b	
	
--Modulo de un vector
modulo:: (Float,Float) -> Float 
modulo (x,y) = sqrt(x^2+y^2)	

--Dados dos rectangulos definir cual tiene area maxima
mayorRectangulo:: (Int,Int) -> (Int,Int) -> (Int,Int)
mayorRectangulo (x,y) (w,z) | x*y >= w*z = (x,y)
							| otherwise = (w,z)
	
--Cuadrante de un punto(dado un punto indica en que cuadrante esta)
cuadrante:: (Int,Int) -> Int
cuadrante (a,b)
		| a>0 && b>0 = 1
		| a<0 && b>0 = 2
		| a<0 && b<0 = 3
		| otherwise = 4
		

--Distancia entre dos puntos
distancia:: (Float,Float) -> (Float,Float) -> Float
distancia (x1,y1) (x2,y2) = sqrt((x1-x2)^2+(y1-y2)^2)	
		

--Numeros Complejos

--Suma de dos numeros complejos		
sumaComplejos:: (Int,Int) ->(Int,Int) -> (Int,Int)
sumaComplejos (x1,x2) (y1,y2) = (x1+y1,x2+y2)


--Producto de dos numeros complejos
productoComplejos:: (Int,Int) -> (Int,Int) -> (Int,Int)
productoComplejos (x1,x2) (y1,y2) = (x1*y1-x2*y2,x1*y2+x2*y1) 

--Conjugado de un número complejo
conjugado:: (Int,Int) -> (Int,Int)
conjugado (a,b) = (a,-b)

--Intercalacion de pares
intercala:: [a]-> [a]-> [a]
intercala [x1,x2] [y1,y2] = [x1,y1,x2,y2]

--Permutacion cíclica de una lista
ciclo:: [a]-> [a]
ciclo [] = []
ciclo xs = last xs : init xs

--Mayor numero de 2 cifras con dos digitos dados
numeroMayor:: Int ->Int ->Int
numeroMayor a b
	| a*10+b>=b*10+a = a*10+b
	| otherwise = b*10+a
	
--Numero de raices de una ecuacion cuadratica
numeroDeRaices:: Int -> Int -> Int -> Int
numeroDeRaices a b c
	| d<0 = 0
	| d==0 = 1 
	| otherwise = 2
	where d = b^2-4*a*c
	
--Raice de las ecuaciones cuadraticas	
raices:: Float -> Float -> Float -> [Float]
raices a b c = [(-b+d)/t,(-b-d)/t]
	where 
		d = sqrt (b^2 - 4*a*c)
		t = 2*a

		
--Área de un triangulo mediante la formula de Heron
areaHeron:: Float -> Float -> Float -> Float 
areaHeron a b c = sqrt(s*(s-a)*(s-b)*(s-c))
	where s = (a+b+c)/2


--Numeros racionales como pares de enteros 

--Forma reducida de un numero racional
formaReducida::(Int,Int) -> (Int,Int)
formaReducida (a,b) = (div a c,div b c)
	where c = gcd a b

--Suma de dos numeros racionales
sumaRacional:: (Int,Int) -> (Int,Int) -> (Int,Int)
sumaRacional  (a,b) (c,d) = sumaRacional' (formaReducida (a,b)) (formaReducida(c,d))

sumaRacional':: (Int,Int) -> (Int,Int) -> (Int,Int)
sumaRacional' (a,b) (c,d) = formaReducida (a*d+c*b,b*d)

--Producto de dos numeros racionales
productoRacional::(Int,Int) -> (Int,Int) ->(Int,Int)
productoRacional (a,b) (c,d) = formaReducida (a*c,b*d)

--Igualdad de numeros racionales
igualdadRacional:: (Int,Int) -> (Int,Int) -> Bool
igualdadRacional (a,b) (c,d) = igualdadRacional' (formaReducida (a,b)) (formaReducida (c,d))

igualdadRacional':: (Int,Int) -> (Int,Int) -> Bool
igualdadRacional' (a,b) (c,d)
	| a== c && b == d = True
	| otherwise = False
	

	


