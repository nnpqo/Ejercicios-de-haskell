--Definir una función que reciba 4 número y devuelva el mayor.
-- Por combinación.
--Por distinción de casos.
mayor4:: Int -> Int -> Int -> Int -> Int
mayor4 x y w z | x>y && x>w && x>z = x
               | y>x && y>w && y>z = y
               | w>x && w>y && w>z = w
               | otherwise = z

mayor:: Int -> Int -> Int
mayor x y = if x>y  
            then x
            else y

mayor4Combinacion:: Int -> Int -> Int -> Int -> Int
mayor4Combinacion x y w z = mayor (mayor x y) (mayor w z)   --4 3 5 1
--Definir una función que reciba una nota y devuelva el mensaje “Aprobado” o “Reprobado”.
nota:: Int -> String
nota n  |n > 50 = "Aprobado"
        |otherwise = "Reprobado"

--Definir una función que reciba una nota y devuelva el mensaje “Excelente“ si la nota
--esta entre 90-100, “Bien” si esta entre 70-89, “Regular” si esta entre 51-69 y mal si esta
--entre 0-50.
notaOpciones:: Int -> String
notaOpciones n  | n<=100 && n>=90 = "Excelente"
                | n<=89 && n>=70 = "Bien"
                | n<=69 && n>=51 = "Regular"
                | n<=50 && n>=0 = "Mal"
                |otherwise = "Nota invalida"

--Definir una función que reciba como argumentos las notas de primer parcial, segundo
--parcial, final y segunda instancia y retorne el mensaje aprobado o reprobado, según el
--caso.
todasNotas a b c d |((a + b)/2 > 50) || c>50 || d==51 = "Aprobado"
                   |otherwise = "Reprobado"

--Definir una función que reciba 16 números y retorne el mayor
mayorDe2:: Int -> Int -> Int
mayorDe2 a b |a>b = a  
             |otherwise = b
mayorDe8:: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
mayorDe8 a b c d e f g h = mayorDe2 (mayorDe2 (mayorDe2 a b) (mayorDe2 c d)) (mayorDe2 (mayorDe2 e f) (mayorDe2 g h))

--Definir una función que reciba un quebrado y devuelva verdad si este es mayor que 1
--y falso en otro caso

quebrado:: Double  -> Bool
quebrado x | x>1 = True
           |otherwise = False

--Definir una función que reciba 2 fechas y devuelva la fecha mayor
fechaMayor:: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
fechaMayor (a1, m1, d1) (a2, m2, d2)  | a1 > a2 && m1 >= m2 && d1 >= d2 = (a1, m1, d1)
                                      | otherwise = (a2, m2, d2)

--Definir una función que reciba 2 fechas y devuelva los años transcurridos
aniosTranscurridos:: (Int, Int, Int) -> (Int, Int, Int) -> Int
aniosTranscurridos (a1, m1, d1) (a2, m2, d2) |a1 > a2 = a1 - a2
                                             |a2 > a1 = a2 - a1

--Definir una función que reciba 2 fechas y devuelva los meses transcurridos
mesesTranscurridos:: (Int, Int, Int) -> (Int, Int, Int) -> Int
mesesTranscurridos (a1, m1, d1) (a2, m2, d2) | a1 == a2 = abs(m1 -m2)
                                             | otherwise = mesesTotal + meses1 + meses2
                                        where 
                                                meses1 = 12 - m1
                                                meses2 = m2 - 1
                                                mesesTotal = (a2 - a1 -1)*12
--Definir una función que reciba 2 fechas y devuelva los días transcurridos
--Definir una función que reciba 2 fechas y devuelva los días, meses y años transcurridos
--Definir una función que reciba un instante (fecha, hora) e incremente el instante en 1 segundo.
--Definir una funcion que reciba 2 fechas y devuelva la posterior
--Defininr una funkcion que reciba 4 fechas y devuelva la posterior (modelar las fechas como tuplas(dd, mm, aa))
--Recibir fechas y que devuelva el posterior
recibirFecha:: (Int, Int, Int) -> (Int, Int, Int)
recibirFecha (d, m, a) 
                            | d < 31 = (d + 1, m, a)
                            | m < 12 = (1, m+1, a)
                            | otherwise = (1, 1, a+1)

