
--Definir una función que devuelva el mayor de 2 números
mayor:: Int -> Int -> Int
mayor x y = if x > y   
            then x
            else y

--Definir una función que reciba 3 números y devuelva el mayor
mayor3:: Int -> Int -> Int -> Int
mayor3 x y z = if x > y && x > z  -- 6 3 1
                then x
                else (if y > z 
                    then y else z) 

mayor32 :: Int -> Int -> Int -> Int
mayor32 a b c = maximum [a, b, c]
--Definir una función que reciba 4 números y devuelva el mayor
mayor4P:: Int -> Int -> Int -> Int -> Int
mayor4P a b c d = maximum [a,b,c,d]
mayor4:: Int -> Int -> Int -> Int -> Int 
mayor4 x y w z = if x>y && x>w && x>z
                    then x
                    else if y>x && y>w && y>z
                        then y
                        else if w>x && w>y && w>z
                            then w
                            else z

--Definir una función que reciba dos exámenes parciales, un final y una segunda
--instancia y devuelva el mensaje “Aprobado”, “Reprobado” o “Abandono” según el
--caso.
--recibeExamenes:: Int -> Int -> Int -> Int -> String
recibeExamenes pp sp final segi = if (pp + sp) == 0 && final == 0
                                    then "Abandono"
                                    else  if (pp + sp) /2 > 50 || final > 50 || segi == 51
                                        then "Aprobado"
                                        else "Reprobado"

--Definir una función que reciba 2 fechas y devuelva la fecha mayor
fechas:: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) 
fechas (a1, m1, d1) (a2, m2, d2) = if a1>a2   
                                    then (a1, m1, d1) 
                                    else if m1>m2   
                                        then (a1, m1, d1)
                                        else if d1>d2   
                                            then (a1, m1, d1)
                                            else (a2, m2, d2)