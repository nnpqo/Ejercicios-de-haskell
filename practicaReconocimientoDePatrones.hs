--Definir una función que reciba una fecha y devuelva el día
devolverDia::(Int,Int,Int)->Int
devolverDia f1@(d1, m1, a1) = d1
--Definir una función que reciba una fecha y devuelv
a el mes
devolverMes::(Int, Int, Int) -> Int
devolverMes f1@(d1, m1, a1) = m1
--Definir una función que reciba una fecha y devuelva el año
devolverAnio::(Int, Int, Int) -> Int
devolverAnio f1@(d1, m1, a1) = a1
--Definir una función que reciba 2 quebrados y devuelva el mayor
recibirQuebrados::(Double, Double) -> Double
recibirQuebrados (x, y) = if x > y
                            then x
                            else y
--Definir una función que reciba 1 quebrado y lo devuelva reducido
quebradoReducido :: (Int, Int) -> (Int, Int)
quebradoReducido (num, denom) = (num `div` mcd, denom `div` mcd)
  where
    mcd = gcd num denom
--Definir una función que reciba 1 quebrado y devuelva su signo como carácter
devolverCaracter:: (Int, Int) -> Char
devolverCaracter (num, denom)
                    |num>0 && denom>0 = '+'
                    |otherwise = '-'
--Definir una función que reciba 3 fechas y devuelva la fecha menor
fechaMenor::(Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
fechaMenor f1@(d1, m1, a1) f2@(d2, m2, a2) f3@(d3, m3, a3) 
                                                        |a1>a2 && a1>a3 = f1
                                                        |a2>a1 && a2>a3 = f2
                                                        |a3>a1 && a3>a2 = f3
                                                        |m1>m2 && m1>m3 = f1
                                                        |m2>m1 && m2>m3 = f2
                                                        |m3>m1 && m3>m2 = f3
                                                        |d1>d2 && d1>d3 = f1
                                                        |d2>d1 && d2>d3 = f2
                                                        |d3>d1 && d3>d2 = f3
                                                            
--Definir una función que reciba 2 horas y devuelva la hora mayor
horaMayor:: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
horaMayor f1@(h1, m1, s1) f2@(h2, m2, s2) 
                                        | h1 > h2 = f1
                                        | otherwise = f2
--Definir una función que reciba 2 instantes y devuelva el mas reciente (Instante es una
--fecha, hora, minuto, segundo)
instanteMasReciente:: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
instanteMasReciente d1@(f1, h1, m1, s1) d2@(f2, h2, m2, s2) 
                                                | f1 > f2 = d1
                                                | f2 > f1 = d2
                                                | h1 > h2 = d1
                                                | h2 > h1 = d2
                                                | m1 > m2 = d1
                                                | m2 > m1 = d2
                                                | s1 > s2 = d1
                                                | s2 > s1 = d2
                                                | otherwise = d2

--Definir una función que reciba un número natural y devuelva el siguiente
recibirNatural:: Int -> Int
recibirNatural x = if x>0
                then x+1
                else x-1 --entrada recibirNatural(-3)

--Definir una función que reciba un quebrado quebrados y devuelva la simplificación
--(modelar el quebrado de quebrados como par de pares ((a,b),(c,d))
simplificacionQuebrados:: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
simplificacionQuebrados ((a, b), (c, d)) = ((a `div` mcd, b `div` mcd), (c `div` mcd1, d `div` mcd1))
                where
                mcd = gcd a b
                mcd1 = gcd c d
                                        