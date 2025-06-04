--Definir una función que compare 2 listas y devuelva True si las listas son
--iguales
iguales [][] = True
iguales (x:xs) (y:ys) = x == y && iguales(xs) (ys)
iguales _ _ = False

--Definir una función que fusione 2 listas ordenadas en una 3ra. ordenada (sin
--necesidad de ordenar).
fusionar [] ys = ys
fusionar xs [] = xs
fusionar (x:xs) (y:ys) 
                     | x >= y = y : fusionar (x:xs) ys  
                     | otherwise = x : fusionar xs (y:ys)
--Definir una función que verifique si una lista de listas podría ser considerada una
--matriz
matriz [] = True
matriz [_] = True 
matriz (x:y:zs) 
                | length x == length y = matriz (y: zs)
                | otherwise = False
--Definir una función que reciba una lista de números y devuelva todos los
--números pares
pares [] = []
pares (x:xs) = if (x `mod` 2 == 0)
               then x: pares xs 
               else pares xs
--Definir una función que reciba una lista de listas y devuelva solo aquellas cuya
--longitud sea par.
esPar [] = [] 
esPar (x:xs) = if (length x `mod` 2 == 0)
                then x:esPar xs 
                else esPar xs
--Definir una función que reciba una lista de listas de números y borre todos los
--números pares de estas listas
eliminarPares [] = []
eliminarPares (x:xs)
                      | x `mod` 2 == 0 = eliminarPares xs
                      | otherwise = x: eliminarPares xs

supEliminar [] = []
supEliminar (x:xs) = eliminarPares x : supEliminar xs
--Definir una función que reciba una lista de listas y devuelva una lista formada
--por los penúltimos elementos de las listas
formarPenultimo [] = []
formarPenultimo (x:xs) = x : formarPenultimo (xs)

penultimoSub [] = []
penultimoSub (x:xs) = formarPenultimo x : penultimoSub xs 
