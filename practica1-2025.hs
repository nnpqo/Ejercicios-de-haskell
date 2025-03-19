--Definir una función que reciba el lado de un cuadrado y devuelva su área.
areaCuadrado x = x * x
--Definir una función que reciba la base y la altura de un rectángulo y devuelva su área y
--su perímetro.
areaRec b a = (b * a, 2*b + 2*a)
--Definir una función que reciba 2 números y devuelva verdad si el primero es mayor
--que el segundo.
mayor x y = x > y
--Definir una función que reciba un número y retorne verdad si este es múltiplo de 2
multiploDos x = x `mod` 2 == 0
--Definir una función que reciba un número y devuelva verdad si este es múltiplo de 2 y
--de 3 al mismo tiempo.
multiploDosyTres x = ((x `mod` 2 == 0) && (x `mod` 3 == 0)) 
--Definir una función que reciba un número y lo devuelva elevado a la potencia 3.
elevadoTres x = x^3
--Definir funciones que reciban un número y lo devuelvan elevado a la potencia
--4,8,10,32.
potencias x = (x^4, x^8, x^10, x^32)
--Definir una función que reciba dos números y una función de orden y devuelva verdad
--si los números obedecen a la función de orden, falso en otro caso.
recibir a b = a < b 
orden a b = recibir a b
--USANDO EXPRESIONES IF
--Definir una función que devuelva el mayor de 2 números
mayorDos x y = if x > y
                then x
                else y
--Definir una función que reciba 3 números y devuelva el mayor
recibirTres a b c = if (a > b) && (a > c) 
                    then a
                    else if (b > a) && (b > c)
                         then b 
                         else c
--Definir una función que reciba 4 números y devuelva el mayor
recibirCuatro w x y z = if (w > x) && (w > y) && (w > z)
                        then w 
                        else if (x > y) && (x > z)
                        then x
                        else if (y > w) && (y > x) && (y > z)
                             then y
                             else z
--Definir una función que reciba dos exámenes parciales, un final y una segunda
--instancia y devuelva el mensaje “Aprobado”, “Reprobado” o “Abandono” según el
--caso.
examenes primer segundo final segIns = if ((primer + segundo) /2 == 0 && final == 0 && segIns == 0)
                                       then "abandono"
                                       else if ((primer + segundo) /2 >= 51)
                                            then "aprobado"
                                            else if (final >= 51)
                                                 then "aprobado"
                                                 else if (segIns == 51)
                                                      then "aprobado"
                                                      else "reprobado"
--Definir una función que reciba 2 fechas y devuelva la fecha mayor
fechas (d,m,a) (d2,m2,a2) = if (a > a2) 
                            then (d,m,a) 
                            else if (a2 > a)
                                 then (d2,m2,a2)
                                 else if (m > m2)
                                      then (d,m,a)
                                      else if (m2 > m)
                                           then (d2,m2,a2)
                                           else if (d > d2)
                                                then (d,m,a)
                                                else (d2,m2,a2)
--Definir una función que reciba 2 fechas y devuelva los años transcurridos
transcurrido (d,m,a) (d2,m2,a2) = if a > a2
                                  then a - a2
                                  else a2 - a
                                  
