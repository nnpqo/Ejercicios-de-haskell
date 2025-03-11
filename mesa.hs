--Definir una función que reciba el lado de un cuadrado y devuelva su área.
areaCuadrado:: Int -> Int
areaCuadrado l = l * l

--Definir una función que reciba la base y la altura de un rectángulo y devuelva su área y
--su perímetro.
areaPerimetro:: Int -> Int -> (Int,Int)
areaPerimetro b a = (area, perimetro)
    where
    area = b * a
    perimetro = 2 * (a + b)

--Definir una función que reciba 2 números y devuelva verdad si el primero es mayor
--que el segundo.
esMayor:: Int -> Int -> Bool
esMayor x y 
            | x > y = True 
            | otherwise = False

--Definir una función que reciba un número y retorne verdad si este es múltiplo de 2.
multiploDos:: Int -> Bool
multiploDos n 
            | n `mod` 2==0 = True
            | otherwise = False

--Definir una función que reciba un número y devuelva verdad si este es múltiplo de 2 y
--de 3 al mismo tiempo.
multiploDosTres:: Int -> Bool
multiploDosTres n = (n `mod` 2 == 0 && n `mod` 3 == 0)

--Definir una función que reciba un número y lo devuelva elevado a la potencia 3.
potenciaTres:: Int -> Int 
potenciaTres n = n ^ 3

--Definir funciones que reciban un número y lo devuelvan elevado a la potencia
--4,8,10,32.
potencias:: Int -> (Int, Int, Int, Int)
potencias n = (potenciaCuatro, potenciaOcho, potenciaDiez, potenciaTrentaDos)
    where
        potenciaCuatro = n ^ 4
        potenciaOcho = n ^ 8
        potenciaDiez = n ^ 10 
        potenciaTrentaDos = n ^ 32

--Definir una función que reciba dos números y una función de orden y devuelva verdad
--si los números obedecen a la función de orden, falso en otro caso.

recibe:: Int -> Int -> Bool
recibe a b = orden a b 
orden:: Int -> Int -> Bool
orden a b 
        | a < b = True
        |otherwise = False

--EXPRESIONES IF
--Definir una función que devuelva el mayor de 2 números
elMayor:: Int -> Int -> Int
elMayor a b = if a > b
                then a 
                else b

--Definir una función que reciba 3 números y devuelva el mayor
tMayor:: Int -> Int -> Int -> Int 
tMayor a b c = if a > b && a > c
                then a
                else if b > a && b > c 
                        then b 
                        else c

--Definir una función que reciba 4 números y devuelva el mayor
mayorCuatro:: Int -> Int -> Int -> Int -> Int 
mayorCuatro a b c d = if a > b && a > c && a > d
                        then a 
                        else if b > c && b > a && b > d 
                                then b 
                                else if c > a && c > b && c > d
                                        then c 
                                        else d

-- Definir una función que reciba dos exámenes parciales, un final y una segunda
--instancia y devuelva el mensaje “Aprobado”, “Reprobado” o “Abandono” según el
--caso.
examen:: Int -> Int -> Int -> Int -> String
examen a b c d = if ((a + b) `div` 2 > 50)
                    then "Aprobado"
                    else if c > 50 
                            then "Aprobado"
                            else if d > 50 
                                    then "Aprobado"
                                    else "Reprobado"

--Definir una función que reciba 2 fechas y devuelva la fecha mayor
fechaMayor:: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
fechaMayor (d1,m1,a1) (d2,m2,a2) = if a1 > a2
                                    then (d1,m1,a1)
                                    else if m1 > m2
                                            then (d1,m1,a1)
                                            else if a1 > a2
                                                    then (d1,m1,a1)
                                                    else (d2,m2,a2)

--Definir una función que reciba 4 número y devuelva el mayor.
--Por combinación.
--Por distinción de casos.
cuatroMayor:: Int -> Int -> Int -> Int -> Int
cuatroMayor a b c d = comparar a b c d 

comparar:: Int -> Int -> Int -> Int -> Int
comparar a b c d 
                | a > b && a > c && a > d = a
                | b > a && b > c && b > d = b 
                | c > a && c > b && c > d = c
                | otherwise = d 

--Definir una función que reciba una nota y devuelva el mensaje “Aprobado” o
-- “Reprobado”.
nota:: Int -> String
nota n 
      | n > 50 = "Aprobado"
      | otherwise = "Reprobado"

--Definir una función que reciba una nota y devuelva el mensaje “Excelente“ si la nota
--esta entre 90-100, “Bien” si esta entre 70-89, “Regular” si esta entre 51-69 y mal si esta
--entre 0-50.
recibirNota:: Int -> String
recibirNota n 
             | n > 89 && n < 101 = "Excelente"
             | n > 69 && n < 90 = "Bien"
             | n > 50 && n < 70 = "Regular"
             | n >= 0 && n < 51 = "Mal"
             | otherwise = "Nota incorrecta"

---- Definir recursivamente una funcion que traduzca una sentencia al lenguaje P, que consiste en
-- reemplazar cada vocal por tres letras: vocalPvocal, por ejemplo, 'e' por "epe"
-- Existen casos especiales como gui, gue, que, qui, donde la u no debe ser tomada en cuenta,
-- por ejemplo "que" se traduce a "quepe", "gu" se traduce a "gupu", "gue" se traduce a "guepe"
-- Ejemplo:
-- traduce "Lo que quise decir es que Gustavo sabe hacer guisos" => "Lopo quepe quipisepe depecipir
--      epes quepe Gupustapavopo sapabepe hapaceper guipisopos"

-- Para la siguiente implementacion no se toman en cuenta vocales en mayuscula.

traductor:: String -> String 
traductor [] = []
traductor ('g':'u':'i': xs) = 'g':'u':'i':'p':'i': traductor xs  
traductor ('g':'u':'e': xs) = 'g':'u':'e':'p':'e': traductor xs 
traductor ('q':'u':'e': xs) = 'q':'u':'e':'p':'e': traductor xs 
traductor ('q':'u':'i': xs) = 'q':'u':'i':'p':'i': traductor xs
traductor (x:xs) 
                | esVocal = x:'p':x: traductor xs 
                | otherwise = x: traductor xs 
                where esVocal = x=='a' || x=='e' || x=='i' || x=='o' || x=='u'






-- Definir una funcion subcadena que reciba una cadena, una posicion inicial ini, una posicion final fin
-- y devuelva la subcadena que va de la posicion ini a la posicion fin, como se ilustra en el siguiente
-- ejemplo:
-- subCadena "abcdefg" 2 5 => "cdef"
-- La funcion debe trabajar con el tipo Cadena en lugar de String y con el tipo PosicionInicial y
-- PosicionFinal en lugar de Int

-- Definición de tipos según la imagen
data Lista a = SinElementos | Aumentar a (Lista a)
data Natural = Nada | Unomas Natural

type Cadena = Lista Char
type PosicionInicial = Natural
type PosicionFinal = Natural

-- Función subcadena que extrae una subcadena desde una posición inicial hasta una posición final
subcadena :: Cadena -> PosicionInicial -> PosicionFinal -> Cadena
subcadena _ Nada Nada = SinElementos
subcadena SinElementos _ _ = SinElementos
subcadena (Aumentar x xs) Nada Nada = Aumentar x SinElementos
subcadena (Aumentar x xs) Nada (Unomas n) = Aumentar x (subcadena xs Nada n)
subcadena (Aumentar _ xs) (Unomas m) (Unomas n) = subcadena xs m n



--Para este problema supondremos que las posiciones de los eementos de la lista, inician
--al final de la misma, como se ilustra a continuacion. Por ejemplo:'
---Posiciones: 6543210
--Cadena:"baacbda"
--La funcion buscaycuenta recibe un caracter y una cadena y devuelve un par (lp,lt) donde 
--lp=lista de posiciones en que el caracter ocurre en la cadena
--lt=cantidad total de elementos en la lista

--buscaYcuenta :: Char-> String -> ([Int], Int)
--buscaYcuenta u v = foldr f a v 
  --      where
     --           a =
       --         f
buscaYcuenta :: Char -> String -> ([Int], Int)
buscaYcuenta u v = foldr f a v
  where
    a = ([], 0)  -- El acumulador inicial es una tupla con una lista vacía y el contador en 0
    f x (pos, cnt)
      | x == u    = (cnt : pos, cnt + 1)  -- Si el carácter coincide, se agrega la posición
      | otherwise = (pos, cnt + 1)        -- Si no, solo se incrementa el contador



