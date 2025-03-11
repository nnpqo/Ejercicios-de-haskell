--Usando funciones estandar sobre listas definir una funcion que compare 2 cadenas 
--devolviendo una lista de tuplas (a,b,c) de lo caracteres y posiciones que difieren:
--Para cada tupla (a,b,c) asumir que 
--a es la posicion 
--b es un caracter de la primera cadena 
--c es un caracter de la segunda cadena 
--ejemplo: comparar "Ana come bananas" "ada come bananos" => [(1,'n','d'), (14,'a','o')]
--Asumir que las cadenas son del mismo tamanio
{-cadenas1:: (Int, String, String) -> Bool
cadenas1 (_,x,y) = x /= y

compararDos:: String -> String -> [(Int, Char, Char)]
compararDos x y = filter cadenas1 (zip3 [0..] x y)-}

-- Definir una funcion subcadena que reciba una cadena, una posicion inicial ini, una posicion final fin
-- y devuelva la subcadena que va de la posicion ini a la posicion fin, como se ilustra en el siguiente
-- ejemplo:
-- subCadena "abcdefg" 2 5 => "cdef"
-- La funcion debe trabajar con el tipo Cadena en lugar de String y con el tipo PosicionInicial y
-- PosicionFinal en lugar de Int
{-type cadenaArgumento = String
type posicionInicial = Int
type posicionFinal = Int 

subCadena "" _ _ = ""
subCadena cadenaArgumento posicionInicial posicionFinal | posicionInicial > posicionFinal = ""
                                                        | posicionFinal > length cadenaArgumento = ""
                                                        | otherwise take (posicionFinal - posicionInicial + 1) (drop (posicionInicial-1) cadenaArgumento)-}

type CadenaArgumento = String
type PosicionInicial = Int
type PosicionFinal = Int 

--subCadena :: CadenaArgumento -> PosicionInicial -> PosicionFinal -> CadenaArgumento
subCadena "" _ _ = ""
subCadena cadenaArgumento posicionInicial posicionFinal
  | posicionInicial > posicionFinal = ""
  | posicionFinal > length cadenaArgumento = ""
  | otherwise = take (posicionFinal - posicionInicial + 1) (drop (posicionInicial - 1) cadenaArgumento)

fun:: ((Int,Int,Char)-> Int -> Int) -> ((Bool,Int) -> Bool) -> (Int->(Bool,Int))
fun f g x = if g (f (x, x+1, 'a') x) 
            then (True, x+1) 
            else (False, x+1)