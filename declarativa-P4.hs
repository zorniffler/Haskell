module Declarativa where

import Data.Char
import Data.List
import Data.Maybe

-- Ejercicios Practica 4.

--Funciones simples antes de terminar el tema 5:

-- Función if_then_else
if_then_else:: Bool -> a -> a -> a
if_then_else True x _ = x
if_then_else False _ y = y

-- Maximo de dos elementos
maximo:: Integer -> Integer -> Integer
maximo a b = if a >=b then a else  b

-- Minimo de dos elementos
minimo:: Integer -> Integer -> Integer
minimo a b = if a <= b then a else  b

-- Numero es par
par:: Int -> Bool
par x = if (mod x 2) == 0 then True else False

-- Numero es impar
impar:: Integer -> Bool
impar x = if mod x 2 /= 0 then True else False

-- Suma de cifras de un entero
cifras_entero:: Int -> Int
cifras_entero n | n<10 = n
cifras_entero x = mod x 10 + cifras_entero (x `div` 10)


-- Potencia de un entero
potencia:: Int -> Int -> Int
potencia  a b = a^b

-- m.c.d 

mcd :: Int -> Int -> Int
mcd n m = mcd m (mod n m)


-- m.c.m 

mcm :: Int -> Int -> Int
mcm n m = div (n*m) (mcd n m)


-- Funciones sobre listas:

-- N-simo elemento de una lista.

nsimo:: [a] -> Int -> a
nsimo [] _ = error "Index too large"
nsimo (x:_) 0 = x
nsimo (_:xs) n = nsimo xs (n-1)


-- N primero elementos de una lista
nprimeros:: Int -> [a] -> [a]
nprimeros n _ | n <= 0 = []
nprimeros n (x:xs) = x: nprimeros (n-1) xs

-- Elementos en posicion par de una lista
posPar:: [a] -> [a]
posPar [] = []
posPar xs = posPar' xs 0


posPar':: [a] -> Int -> [a]
posPar' [] _ = []
posPar' (x:xs) n = if mod n 2 == 0 then x: posPar' xs (n+1)
				else posPar' xs (n+1)

-- Buscar y sustituir un elemento en una lista.
sustituir::(Eq a) => [a]-> a -> a ->[a]
sustituir [] a b = []
sustituir (h:t) elem1 elem2 = if h == elem1 then sustituir (elem2:t) elem1 elem2 else h:(sustituir t elem1 elem2)

-- Mayor elemento de una lista
mayorList::(Ord a)=> [a] -> a
mayorList [] = error "Lista vacia"
mayorList (h:u:xs) = if h>=u then mayorList (h:xs) else mayorList (u:xs)


-- Ejercicio 3

esPrimoQ :: Int -> Bool
esPrimoQ x = if((length[n | n <- [1..x], mod x n==0]) ==2) then True else False



-- Ejercicio 4

calculateToBase:: String -> Int -> Int
calculateToBase "" _ = error "No numero"
calculateToBase s b = calculateToBase2 (stringToList s) b

calculateToBase2:: [Int] -> Int -> Int
calculateToBase2 [] _ = 0
calculateToBase2 xs b = sum $ zipWith(*) (reverse xs) $ iterate (b*) 1


stringToList:: String -> [Int]
stringToList = map convertHex1 


-- Forma habitual aprendida en 2º
convertHex:: Char -> Int
convertHex a = fromJust (elemIndex a b)
			where b = ['0'..'9'] ++ ['A'..'Z']


convertHex1 :: Char -> Int
convertHex1 = fromJust . (`elemIndex` b)
			where b = (['0'..'9'] ++ ['A'..'Z'])


-- Ejercicio 5

intToBase:: Int ->Int -> String
intToBase ent b = reverse (intToBase' ent b)

intToBase':: Int -> Int -> String
intToBase' ent b 
		| ent < b =  [str !! ent]
		| otherwise =  (str !! (mod ent b)):(intToBase' (div ent b) b)
		where
			str = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"



-- Ejercicio 6

primoshasta :: Int -> [Int]
primoshasta n = [x | x<-[2..n], esPrimoQ x]			
			
			
			
-- Ejercicio 7

trenza::[[a]] -> [a]
trenza ([]) = []
trenza ([]:s) = trenza s
trenza ((h:t):s) = h : trenza (s ++ [t])



-- Ejercicio 8

distribuye::(Num a) => [Int] -> [[Int]]
distribuye [] = [[],[],[]]
distribuye (h:t)
	| h<10 = [(h:l1),l2,l3]
	| h<100 = [l1,(h:l2),l3]
	| otherwise = [l1,l2,(h:l3)]
	where [l1,l2,l3] = distribuye t	



distribuye' :: (Num a) => [Int] -> [[Int]]
distribuye' [] = [[],[],[]]
distribuye' (h:t) 
	| h<10 =  distribuye2 [(h:[]),[],[]] t
	| h<100 = distribuye2 [[],(h:[]),[]] t
	| otherwise = distribuye2 [[],[],(h:[])] t			




distribuye2 :: (Num a) =>[[Int]]-> [Int] -> [[Int]]
distribuye2 [l1,l2,l3] [] = [l1,l2,l3]
distribuye2 [l1,l2,l3](h:t) 
	| h<10 =  distribuye2 [(h:l1),l2,l3] t
	| h<100 = distribuye2 [l1,(h:l2),l3] t
	| otherwise = distribuye2 [l1,l2,(h:l3)] t			




-- Ejercicio 9

comb:: Int -> Int -> Int
comb m n 
	| n == 0 || m == n = 1
	| otherwise = comb (m-1) n +comb (m-1) (n-1)

-- Ejercicio 10 
trianguloPascal :: (Integral a) => a -> [a]
trianguloPascal 0 = [1]
trianguloPascal x = 1:trianguloPascal' (trianguloPascal (x-1))	
	
	
trianguloPascal' :: (Integral a) => [a] -> [a]
trianguloPascal' [_] = [1]
trianguloPascal' (x:y:ys) = (x + y): trianguloPascal' (y:ys)	




-- Ejercicio 11.
menorList':: (Ord a) => [a] -> a
menorList' (h:[]) = h
menorList' (h:h1:t)
	| h<h1 = menorList' (h:t)
	| otherwise = menorList' (h1:t)


menorList:: (Ord a) => [a] ->[a]
menorList (h:[]) = []
menorList xs = menorList2 xs (menorList' xs)

menorList2 :: (Ord a) => [a] -> a -> [a]
menorList2 (h:t) elem
	| h== elem = t
	| otherwise = h:(menorList2 t elem)


	
	
-- Ejercicio 12
factorial:: Int -> Int
factorial 0 = 1
factorial n = n*factorial(n-1)


listFactorial:: Int -> [Int]
listFactorial n = reverse[factorial n | n <-[0..n-1]]


-- Uso de factorial aplicando let.
listFactorial' :: Int -> [Int]
listFactorial' 1 = [1]
listFactorial' n = let (h:t) = listFactorial' (n-1)
				   in scanl (*) 1 [1..n-1]



-- Ejercicio 13

-- Primera Opción
pasada :: (Ord a) => [a] -> [a]
pasada [] = []
pasada [x] = [x]
pasada (x:y:t)= if y < x then y : pasada (x:t) else x : pasada (y:t)
	
pasadaNum :: (Ord a) => [a] -> Int
pasadaNum [] = 0
pasadaNum [x] = 0	
pasadaNum (x:y:t) = if y < x then 1+pasadaNum (x:t) else 0+ pasadaNum(y:t)


pasadaGen ::(Ord a)=> [a] -> ([a],Int)
pasadaGen xs = (pasada xs, pasadaNum xs)


data Moneda = Euro | Peseta | Lira | Marco | Franco | Florin | Escudo | Dolar | Yen | Libra deriving Show  -- etc

type Precio = (Moneda, Float)

unEuro :: Moneda -> Float
unEuro Euro = 1.0
unEuro Peseta = 166.386 -- valor en pesetas de 1 Euro
unEuro Lira = 1936.27 -- valor en liras de 1 Euro
unEuro Marco = 1.95583 -- valor en marcos de 1 Euro
unEuro Franco = 6.55957 -- etc.
unEuro Florin = 2.20371
unEuro Escudo = 200.48
unEuro Dolar = 0.885
unEuro Yen = 111.360

-- a) f:: (Moneda,Precio) -> [Moneda]

--prueba:: x+(if x>y then x else y) where [x,_,y] = pasada [2,3,1]


-- b) se reduce a 5.

-- e) Cambiar precio dado en una moneda a otra cualquiera 

cambioMoneda :: Precio -> Moneda -> Precio
cambioMoneda (m,v) a = (a,v /(unEuro m)*euros)
		where euros = unEuro a 
	

-- EJERCICIOS EXAMEN.

cifras :: Integer -> [Integer]
cifras n = if (n<10) 
           then [n]
           else (mod n 10) : cifras (div n 10)    

		   		   
capicua :: Integer -> Bool
capicua n = (r == reverse r)
            where r = cifras n











