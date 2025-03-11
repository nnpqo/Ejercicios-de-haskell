--Usando cálculo lambda definir todas las funciones de la presente práctica
--Definir una función que reciba el lado de un cuadrado y devuelva su área.
areaCuadrado:: Num a => a -> a
areaCuadrado = (\lado -> lado * lado)
----Definir una función que reciba la base y la altura de un rectángulo y devuelva su área y
--su perímetro.
areaPerimetroRectangulo:: Num a => a -> a -> (a,a)
areaPerimetroRectangulo = (\base -> \altura -> (base * altura, (2*base + 2*altura)))
----Definir una función que reciba 2 números y devuelva verdad si el primero es mayor
--que el segundo.
mayor:: Int -> Int -> Bool
mayor =(\x -> \y -> x > y)
--Definir una función que reciba un número y retorne verdad si este es múltiplo de 2.
multiplo:: Int -> Bool
multiplo =(\x ->(x `mod` 2 == 0))
--Definir una función que reciba un número y devuelva verdad si este es múltiplo de 2 y
--de 3 al mismo tiempo.
multiplo2y3:: Int -> Bool
multiplo2y3 = (\x -> (x `mod` 2 == 0) && (x `mod` 3 == 0))
--Definir una función que reciba un número y lo devuelva elevado a la potencia 3.
potencia:: Int -> Int
potencia = (\x -> ((^) x 3))
--Definir funciones que reciban un número y lo devuelvan elevado a la potencia
--4,8,10,32.
potencia4:: Int -> Int
potencia4 = (\x -> (x ^ 4))

potencia8:: Int -> Int
potencia8 = (\x -> (x ^ 8))

potencia10:: Int -> Int
potencia10 = (\x -> (x ^ 10))

potencia32:: Int -> Int
potencia32 = (\x -> (x ^ 32))

potenciaMultiple:: Int -> (Int, Int, Int, Int)
potenciaMultiple =(\y -> (potencia4 y, potencia8 y, potencia10 y, potencia32 y))

--Definir una función que reciba dos números y una función de orden y devuelva verdad
--si los números obedecen a la función de orden, falso en otro caso.
funcionOrden:: Int -> Int -> Bool
funcionOrden = (\x -> \y -> x > y)

ordenTotal:: Int -> Int -> (Int -> Int -> Bool) -> Bool
ordenTotal = (\x -> \y -> \funcionOrden -> funcionOrden x y)

--Definir una función que devuelva el mayor de 2 números
mayorConLamda:: Int -> Int -> Int
mayorConLamda = (\x -> \y -> if x > y  
                     then x
                     else y)

--Definir una función que reciba 3 números y devuelva el mayor
mayor3:: Int -> Int -> Int -> Int
mayor3 = (\x -> \y -> \z -> if x > y && x > z 
                then x
                else (if y > z 
                    then y else z))

--Definir una función que reciba 4 números y devuelva el mayor
mayor4P:: Int -> Int -> Int -> Int -> Int
mayor4P = (\a -> \b -> \c -> \d -> maximum [a,b,c,d])

mayor4:: Int -> Int -> Int -> Int -> Int 
mayor4 = (\x -> \y -> \w -> \z -> if x>y && x>w && x>z
                    then x
                    else if y>x && y>w && y>z
                        then y
                        else if w>x && w>y && w>z
                            then w
                            else z)

--Definir una función que reciba dos exámenes parciales, un final y una segunda
--instancia y devuelva el mensaje “Aprobado”, “Reprobado” o “Abandono” según el
--caso.
--recibeExamenes:: Int -> Int -> Int -> Int -> String
recibeExamenes = (\pp -> \sp -> \final -> \segi -> if (pp + sp) == 0 && final == 0
                                    then "Abandono"
                                    else  if (pp + sp) /2 > 50 || final > 50 || segi == 51
                                        then "Aprobado"
                                        else "Reprobado")

--Definir una función que reciba 2 fechas y devuelva la fecha mayor
fechas:: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) 
fechas = (\(a1, m1, d1) -> \(a2, m2, d2) -> if a1>a2   
                                    then (a1, m1, d1) 
                                    else if m1>m2   
                                        then (a1, m1, d1)
                                        else if d1>d2   
                                            then (a1, m1, d1)
                                            else (a2, m2, d2))