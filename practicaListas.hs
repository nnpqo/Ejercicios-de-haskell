--map (even.head) [[1,2,3,4],[6,7,1,2],[7,8]]
resultado x = map (even.head) x
--filter ((>2).head) [[1,2,3,4],[6,7,1,2],[7,8]]
resultado1 x = filter ((>2).head) x
--takeWhile (even.head) [[1,2,3,4],[6,7,1,2],[7,8]]
resultado2 x = takeWhile (even.head) x
--dropWhile (even.head) [[1,2,3,4],[6,7,1,2],[7,8]]
resultado3 x = dropWhile (even.head) x
--((\(x,y) -> y) .last ) (zip [1,2,3,4,5][map (*2), filter (>2)] ) [1,2,6,8,3]
--filter (even.length) (map (map (even.head.tail.tail)) [[[1,2,4,5,6],[8,3,5,2],[2,2,2]], [[1,2,4,5,6],[8,3,5,2]]])
--map (\(x,y) -> x y) (zip [takeWhile even,filter odd,dropWhile (even.(+3))] [[2,7,8],[3,6,5,4],[2,5,7]])
--map (map (head.tail.tail.init)) [[[2,7,8,1,3],[3,6,5,4,6,3],[2,5,7,1]], [[2,7,8,9,10],[1,2,3,6,5,4],[3,8,2,5,7]]]
--(head.tail.tail) [(+5),(*6),((*3).(+4))] 10
--last [(+5),(*6),((*3).(+4))] 10
--(head.tail.tail) [(+),(*),((*3).(+4))] 10
--last [(+1),(*2),((*3).(+4))] 10
--map (map (map (*2))) [[[1,2,3],[4,5]],[[6,7]],[[8],[9,10]]]

--Utilizando las funciones estándar sobre listas, definir las siguientes funciones:
--1. Definir una función que reciba una lista de elementos y devuelva el tercero
tercerElemento:: [a] -> a
tercerElemento x = head(drop 2 x)
--Definir una función que reciba una lista de elementos y devuelva el segundo
segundoElemento:: [a] -> a
segundoElemento x = last(take 2 x)
--Definir una función que reciba una lista de listas de funciones y un elemento y aplique la 1ra función
--de la primera lista al elemento


--Definir una función que reciba una lista de listas y devuelva el 5to. Elemento de la 3ra. lista.
quintoElemento:: [[a]] -> a
quintoElemento x = head(drop 4 (head(drop 2 x)))
--quintoElemento x = head(drop 4 (x !! 2))
--Definir una función que reciba una lista de listas de listas y devuelva el 3er. elemento de la 4ta. Lista
--de la 2da. lista
tercerElemento2::[[[a]]] -> a
tercerElemento2 x = head (drop 2 (head(drop 3 (x !! 1))))
--Definir una función que compare 2 listas y devuelva True si las listas son iguales
compararListas :: Ord a => [a] -> [a] -> Bool
compararListas x y = length x == length y && and (zipWith (==) x y)
--Definir una función que verifique si una lista de listas podría ser considerada una matriz

--Definir una función que reciba un número y una lista y devuelva el elemento de la lista que esta en la posición n
recibirNumero:: Int -> [a] -> a 
recibirNumero n x =  x !! (n-1)
--Definir las funciones length, filter, zip utilizando las otras funciones
--Definir una función que reciba una matriz y devuelva su transpuesta
                --matrizTranspuesta x | map head x ++ matrizTranspuesta (map tail x)
--Definir una función que reciba 2 matrices y las multiplique
--Definir una función que reciba 3 matrices y las multiplique
--Definir una función que reciba 4 matrices y las multiplique
--Definir una función que reciba 1 matriz y una función de orden y devuelva True si la matriz esta ordenada de acuerdo a la función de orden.
--Definir una función que reciba una lista de números y devuelva todos los números pares
--Definir una función que reciba una lista de listas y devuelva una lista de sus longitudes.
--Definir una función que reciba una lista de listas y devuelva solo aquellas cuya longitud sea par.
--Definir una función que reciba una lista de listas de números y borre todos los números pares de estas listas
--Definir una función que reciba una lista de listas y devuelva una lista formada por los penúltimos elementos de las listas
--Definir una función que reciba un número y devuelva una lista con los posibles divisores del número.
--Definir la función zipWith en terminos de zip

--Sea:
--(map f).(map g) xs = map p xs
--De una definición apropiada para p
--Sea:
--(filter q)  (filter p) = filter (p  q)
--de una definición apropiada para el operador 
--[[[1,2], [3,4,5], []], [[]], []] :: [[[Int]]]
--[[]]::[[a]]
--[[[]]]::[[[b]]]
--[1, []]::No es una lista
--[[[1,2,3], []], [[[]]]]::No es una lista
f1 = head[[1,2,3]]
f2 = head [[[]]]
f3 = head [[], [[1,2,7]]]
f4 = tail [[1,2,3]]
f5 = tail [[[]]]
f6 = tail [[]]
f7 = tail []
f8 = tail [1]
f9 = init [[1,2,3]]
f10 = init [[[]]]
f11 = init [[]]
f12 = init [[1, 2, 3], [4,5], []]
f13 = take 0 [1, 10, 3]
f14 = take 2 [[1,2,7,3,4]]
f15 = take 5 [[[True, False]]]
f16 = zip [[1,2,7,8,9]]['a', 'b', 'c', 'd']
--f17 = zip [(>),(<)] [2,3,4]








