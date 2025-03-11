--Definir una función que reciba 6 números y devuelva el menor
menor a b = if a < b
            then a
            else b

devolverMenor:: Int -> Int -> Int -> Int -> Int -> Int -> Int
devolverMenor a b c d e f = 
    let res = menor (menor a b) (menor c d)
    in if res < (menor e f)
        then res
        else (menor e f)

--Definir una función que reciba 3 números y devuelva el mensaje “Sumatoria mayor” si
--la sumatoria de los números es menor que 20, el mensaje “Sumatoria menor” si la
--sumatoria es menor que 10 y el mensaje “Vacio” en otro caso.

suma n1 n2 n3 = (n1+n2+n3)
sumatoriaTres:: Int -> Int -> Int -> String
sumatoriaTres n1 n2 n3 =
    let m = suma n1 n2 n3
    in case () of
       _|m<=20 && m>=10 -> "Sumatoria mayor"
        |m<10 && m>=0 -> "Sumatoria menor"
        |otherwise -> "Vacio" 

--Definir una función que reciba 3 notas que devuelva el mensaje “Excelente“ si el
--promedio esta entre 90-100, “Bien” si esta entre 70-89, “Regular” si esta entre 51-69 y
--mal si esta entre 0-50.

sumatoriaP n1 n2 n3 = (n1+n2+n3)/3
promedioTres:: Double -> Double -> Double -> String
promedioTres n1 n2 n3 =
    let m = sumatoriaP n1 n2 n3
    in case () of
        _|m>=90 && m<=100 -> "Excelente"
         |m>=70 && m<=89 -> "Bien"
         |m>=51 && m<=69 -> "Regular"
         |m>=0 && m<=50 -> "Mal"
         |otherwise -> "nota invalida"

