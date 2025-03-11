--Definir una función que reciba el lado de un cuadrado y devuelva su área.
areaCuadrado:: Int -> Int
areaCuadrado lado = lado * lado
--Definir una función que reciba 2 números y devuelva verdad si el primero es mayor
--que el segundo.
identificarMayor:: Int -> Int -> Bool
identificarMayor x y = x > y 
--Definir una función que reciba un número y retorne verdad si este es múltiplo de 2
multiplo2:: Int -> Bool
multiplo2 num = (num `mod` 2 == 0)
--Definir una función que reciba un número y devuelva verdad si este es múltiplo de 2 y
--de 3 al mismo tiempo.
multiplo2y3:: Int -> Bool
multiplo2y3 num = (num `mod` 2 == 0) && (num `mod` 3 == 0)
--Definir una función que reciba un número y lo devuelva elevado a la potencia 3.
potencia3:: Int -> Int
potencia3 num = num ^ 3
--Definir funciones que reciban un número y lo devuelvan elevado a la potencia
--4,8,10,32.
potenciasMultiples:: Int -> (Int, Int, Int, Int)
potenciasMultiples num = (num ^ 4, num ^ 8, num ^ 10, num ^ 32)
--Definir una función que reciba dos números y una función de orden y devuelva verdad
--si los números obedecen a la función de orden, falso en otro caso.
orden:: Int -> Int -> Bool
orden numero1 numero2 = numero1 > numero2

funcion::(Int -> Int -> Bool) -> Int -> Int -> Bool
funcion orden num1 num2 = orden num1 num2