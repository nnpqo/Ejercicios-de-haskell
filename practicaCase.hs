--Definir una función que reciba una vocal y retorne la siguiente
vocal:: Char -> Char
vocal x =
    case x of
        'a' -> 'e'
        'e' -> 'i'
        'i' -> 'o'
        'o' -> 'u'
        'u' -> 'a'
--Definir una función que reciba un dígito y retorne su literal
digito:: Int -> String
digito x =
    case x of
        0 -> " "
        1 -> "Uno"
        2 -> "Dos"
        3 -> "Tres"
        4 -> "Cuatro"
        5 -> "Cinco"
        6 -> "Seis"
        7 -> "Siete"
        8 -> "Ocho"
        9 -> "Nueve"
        otherwise -> "Digito invalido"
--Definir una función que reciba un número de dos dígitos y retorne su literal
literalDecenas:: Int -> String
literalDecenas n = 
    case n of 
        0 -> " "
        1 -> "Dieci"
        2 -> "Veinti"
        3 -> "Treinta"
        4 -> "Cuarenta"
        5 -> "Cincuenta"
        6 -> "Sesenta"
        7 -> "Setenta"
        8 -> "Ochenta"
        9 -> "Noventa"
        otherwise -> "numero invalido"

operador:: Int -> String   
operador num = literalDosCifras d u
    where
    d = div num 10
    u = mod num 10
literalDosCifras:: Int -> Int -> String
literalDosCifras d u = (literalDecenas d) ++ " y " ++ (digito u)
--hacer con 6 digitos = llamar a los de 3 digitos Mil 3 digitos 
--Definir una función que reciba un número de tres dígitos y retorne su literal
literalTresCifras n =
    case n of
        0 -> " "
        1 -> "Ciento "
        2 -> "Doscientos "
        3 -> "Trescientos "
        4 -> "Cuatrocientos "
        5 -> "Quinientos "
        6 -> "Seiscientos "
        7 -> "Setecientos "
        8 -> "Ochocientos "
        9 -> "Novecientos "
        otherwise -> "numero invalido"

operadorTres:: Int -> String
operadorTres num = literalTres c d u
                where 
                c = div num 100
                d = mod (div num 10) 10
                u = mod num 10

literalTres:: Int -> Int -> Int -> String
literalTres c d u = (literalTresCifras c) ++ (literalDecenas d) ++ " y " ++ (digito u)

--Suponiendo que representamos los valores lógicos por 1 y 0 (true y false), definir una
--función que reciba dos valores lógicos y retorne el resultado de aplicarle la operación
--and
logicoAnd:: Int -> Int -> Bool
logicoAnd x y =
    case (x, y) of
        (1, 0) -> False  
        (0, 1) -> False
        (1, 1) -> True
        (0, 0) -> False

--Idem a 3 pero para or
logicoOr:: Int -> Int -> Bool
logicoOr x y =
    case (x, y) of
        (1, 0) -> True 
        (0, 1) -> True
        (1, 1) -> True
        (0, 0) -> False

--Idem a 3 para xor
logicoXor:: Int -> Int -> Bool
logicoXor x y =
    case (x, y) of
        (1, 0) -> True 
        (0, 1) -> True
        (1, 1) -> False
        (0, 0) -> False

--Idem a 3 pero que reciba como argumento la operación que se realizará.
operacion o x y =
    case o of
        1 -> logicoAnd x y
        2 -> logicoOr x y
        3 -> logicoXor x y

--Definir una función que reciba dos números y retorne el menor
menorDos:: Int -> Int -> Int
menorDos x y = 
    case x<y of
        True -> x
        False -> y

--Definir una función que reciba 6 números y devuelva el menor

menorSeis a b c d e f =
    case menorDos a (menorDos b (menorDos c (menorDos d (menorDos e f)))) of
        res -> res 

--Definir una función que reciba 3 números y devuelva el mensaje “Sumatoria mayor” si
--la sumatoria de los números es menor que 20, el mensaje “Sumatoria menor” si la
--sumatoria es menor que 10 y el mensaje “Vacio” en otro caso.
recibirTresSum:: Int -> Int -> Int -> String
recibirTresSum a b c =
    case () of
        _|contenedor < 20 && contenedor > 10 -> "Sumatoria mayor"
         |contenedor < 10 -> "Sumatoria menor"
         |otherwise -> "Vacio"
    where contenedor = a+b+c

--Definir una función que reciba 3 notas que devuelva el mensaje “Excelente“ si el
--promedio esta entre 90-100, “Bien” si esta entre 70-89, “Regular” si esta entre 51-69 y
--mal si esta entre 0-50.
promedio:: Double -> Double -> Double -> Double
promedio n1 n2 n3 = (n1+n2+n3)/3
notasPromedio:: Double -> Double -> Double -> String
notasPromedio n1 n2 n3 = 
    case () of
        _|sum>=90 && sum<=100 -> "Excelente"
         |sum>=70 && sum<=89 -> "Bien"
         |sum>=51 && sum<=69 -> "Regular"
         |sum>=0 && sum<=50 -> "Mal"
         |otherwise -> "promedio invalido"
        where sum = promedio n1 n2 n3


