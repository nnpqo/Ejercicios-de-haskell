--Calcular el factorial de un numero
factorial:: Int -> Int
factorial(0) = 1
factorial(n) = n * (factorial(n-1))
--Definir una función que compare 2 listas y devuelva True si las listas son iguales
listaIgual:: Eq a => [a] -> [a] -> Bool --Eq se utiliza para comparar la igualdad de valores y se requiere cuando se utiliza el operador ==.
listaIgual [] [] = True
listaIgual [] _ = False
listaIgual _ [] = False
listaIgual (x:xs) (y:ys) = (x == y && listaIgual xs ys)
--Definir una función que fusione 2 listas ordenadas en una 3ra. ordenada (sin necesidad de ordenar).
fusionLista [] [] = []
fusionLista [] _ = []
fusionLista _ [] = []
fusionLista (x:xs) (y:ys) | x <= y = x: fusionLista xs (y:ys)
                          | otherwise = y: fusionLista (x:xs) ys
--Definir una función que reciba una lista de números y devuelva todos los números pares
listaPares [] = []
listaPares (x:xs) 
                  | x `mod` 2 == 0 = x: listaPares xs
                  | otherwise      = listaPares xs          
--Definir una función que reciba una lista de listas y devuelva solo aquellas cuya longitud sea par.
tamanioLista [] = 0
tamanioLista (x:xs) = 1 + tamanioLista xs

listaListasPar [] = []
listaListasPar (x:xs) | tamanioLista x `mod` 2 == 0 = x: listaListasPar xs 
                      | otherwise = listaListasPar xs

--Definir una función que reciba una lista de listas de números y borre todos los números pares de estas listas
borrarElemento:: [Int] -> [Int]
borrarElemento [] = []
borrarElemento (y:ys) | y `mod` 2 /= 0 = y: borrarElemento ys
                      | otherwise = borrarElemento ys
listaBorrar:: [[Int]] -> [[Int]]
listaBorrar [] = []
listaBorrar (x:xs) = borrarElemento x: listaBorrar xs
                          
--Definir una función que reciba una lista de listas y devuelva una lista formada por los penúltimos elementos de las listas
penultimo [] = 0
penultimo [x, _] = x
penultimo (y:ys) = penultimo ys

listaFormadaPenultimo [] = []
listaFormadaPenultimo (x:xs) = penultimo x: listaFormadaPenultimo xs 
--Definir una función que reciba un número y devuelva una lista con los posibles divisores del número.
divisores:: Int -> [Int]
divisores numero = [x | x <- [1..numero], numero `mod` x == 0]
--Definir una función que busque un elemento en una lista mediante búsqueda
--secuencial (la función debería devolver la posición donde se encuentra el
--elemento).
busquedaElemento numero [] = 0
busquedaElemento numero lista = busquedaElemento2 numero lista 1

busquedaElemento2 numero [] _ = 0
busquedaElemento2 numero (x:xs) posicion | numero == x = posicion
                                         | otherwise = busquedaElemento2 numero xs (posicion + 1) 
                                  
buscarElementoOtro e [] = 0
buscarElementoOtro e lista = busqOtro e lista 0

busqOtro e [] posicion = -1
busqOtro e (x:xs) posicion | e == x = posicion
                           | otherwise = busqOtro e xs (posicion+1)
--definir recursivamente una funcion que traduzca una sentencia al lenguaje P, 
--que consiste en reemplazar cada vocal por 3 letras: vocalPvocal, por ejemplo e por epe.  
--Existen casos especiales como gui, gue, que, qui donde la u no debe ser tomada en cuenta, por ejemplo, 
--que se traduce a quepe, gu se traduce a gupu, gue se traduce a guepe. Ejemplo: traduce: 
--"Lo que quise decir es que gustavo sabe hacer guisos" =>  
--"Lopo quepe quipisepe depecipir epes quepe gupustapavopo sapabepe hapaceper guipisopo" 
traducirALenguajeP :: String -> String
traducirALenguajeP = concatMap traducirPalabra . words

traducirPalabra :: String -> String
traducirPalabra palabra
  | palabra `elem` ["gui", "gue", "que", "qui"] = traducirPalabraEspecial palabra
  | otherwise = concatMap traducirLetra palabra

traducirPalabraEspecial :: String -> String
traducirPalabraEspecial "gui" = "gupu"
traducirPalabraEspecial "gue" = "guepe"
traducirPalabraEspecial "que" = "quepe"
traducirPalabraEspecial "qui" = "quipi"
traducirPalabraEspecial _ = error "Palabra especial no válida"

traducirLetra :: Char -> String
traducirLetra 'a' = "apa"
traducirLetra 'e' = "epe"
traducirLetra 'i' = "ipi"
traducirLetra 'o' = "opo"
traducirLetra 'u' = "upu"
traducirLetra letra = [letra]

--potencia de un numero, x elevado a la n
potencia x n  | n == 0    = 1
              | otherwise = x * potencia x (n-1)
--sumar 2 numeros 
suma x 0 = x
suma x y = suma (x + 1) (y - 1)
--operador >

--operador ==
    
--cociente de la division entera 
cociente x y | x<y = 0
             | otherwise = 1 + cociente (x - y) y
--residuo de la division entera
residuoDivision x y | x < y     = x
                    | otherwise = residuoDivision (x - y) y 
--funcion sobre lista takeWhile
tWhile f [] = []
tWhile f (x:xs) = if f x 
                  then x: tWhile f xs
                  else []
-- definir funcion sobre lista foldr recursivamente
myfoldr f argumento1 [] = argumento1
myfoldr f argumento1 (x:xs) = f x (myfoldr f argumento1 xs) 

--definir filter recursivamente 
myfilter f [] = []
myfilter f (x:xs) = if f x 
                    then x : myfilter f xs
                    else myfilter f xs

--definir recursivamente una funcion que traduzca una sentencia al lenguaje P, 
--que consiste en reemplazar cada vocal por 3 letras: vocalPvocal, por ejemplo e por epe.  
--Existen casos especiales como gui, gue, que, qui donde la u no debe ser tomada en cuenta, por ejemplo, 
--que se traduce a quepe, gu se traduce a gupu, gue se traduce a guepe. Ejemplo: traduce: 
--"Lo que quise decir es que gustavo sabe hacer guisos" =>  
--"Lopo quepe quipisepe depecipir epes quepe gupustapavopo sapabepe hapaceper guipisopo" 

traducirCompleto :: String -> String
traducirCompleto [] = ""
traducirCompleto (x:xs)
  | [x] `elem` ["a", "e", "i", "o", "u"] = traducir x ++ traducirCompleto xs
  | (take 3 (x:xs)) `elem` ["gui", "gue", "que", "qui"] = traducirPalabraEspecial2 (take 3 (x:xs)) ++ traducirCompleto (drop 3 (x:xs))
  | otherwise = x : traducirCompleto xs 

traducirPalabraEspecial2:: String -> String
traducirPalabraEspecial2 "gui" = "guipi"
traducirPalabraEspecial2 "gue" = "guepe"
traducirPalabraEspecial2 "que" = "quepe"
traducirPalabraEspecial2 "qui" = "quipi"
traducirPalabraEspecial2 palabraT = palabraT

traducir:: Char -> String
traducir 'a' = "apa"
traducir 'e' = "epe"
traducir 'i' = "ipi"
traducir 'o' = "opo"
traducir 'u' = "upu"
traducir letra = [letra]



