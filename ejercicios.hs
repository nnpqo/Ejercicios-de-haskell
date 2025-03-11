doble x = 2*x
area lado = lado * lado
primero(x,y) = x
mas x y = (+) x y
suma x y = x + y
--areaRectangulo base altura = (base * altura, perimetro) 
--perimnetro longitud ancho = 2*longitud + 2*ancho
areaPerimetroRectangulo longitud ancho base altura = (base * altura, (2*longitud + 2*ancho))
--las funciones polimorficas trabajan con cualquier tipo de dato
--int, char, bool
--max :: (Ord a) => a
--poten4 x = cuadrado (cuadrado x)

--mayor x y = if x>y
--		then x
--		else y
------------------------------------------mayDe3 n1 n2 n3 
--mayorde3 n1 n2 n3 = mayor(mayor n1 n2) n3
------------------------------------
--esIgual x y = (==) x y
------------------------------------------
--fecMay :: (Int, Int, Int)-> (Int, Int, Int)->(Int, Int, Int)

--definir una funcion que reciba 4 fechas y devuelva el mayor 
mayor:: Int -> Int -> Bool
mayor x y = x > y

multiplo:: Int -> Bool
multiplo x =  x `mod` 2 == 0

multiplo2y3:: Int -> Bool
multiplo2y3 x = (x `mod` 2 == 0) && (x `mod` 3 == 0)

potencia:: Int -> Int
potencia x = (^) x 3

--Definir funciones que reciban un número y lo devuelvan elevado a la potencia
--4,8,10,32.
potencia4:: Int -> Int
potencia4 x = x ^ 4

potencia8:: Int -> Int
potencia8 x = x ^ 8

potencia10:: Int -> Int
potencia10 x = x ^ 10

potencia32:: Int -> Int
potencia32 x = x ^ 32

potenciaMultiple:: Int -> (Int, Int, Int, Int)
potenciaMultiple y =(potencia4 y, potencia8 y, potencia10 y, potencia32 y)

--Definir una función que reciba dos números y una función de orden y devuelva verdad
--si los números obedecen a la función de orden, falso en otro caso.
orden:: Int -> Int -> Bool
orden x y = x < y 

suma3 :: Int -> (Int -> (Int -> Int))
((suma3 x) y) z = x+y+z

f1::(a -> b) -> a -> b
f1 x y = x y

f2:: a -> a
f2 x = x