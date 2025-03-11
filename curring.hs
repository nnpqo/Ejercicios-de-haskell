--div n d = (/) n d 
--dividir: Int -> Int -> Int
--dividir n d = div n d
doble x = 2*x
f1::(ty -> tr) -> ty -> tr
f1 x y = x y
f2:: tx -> tx
f2 x = x
{-
--escribir el tipo de f1(&&)
(&&):: Bool -> Bool -> Bool
(ty -> tr) puede emparejar con (Bool -> (Bool -> Bool))
ty = Bool
tr = (Bool -> Bool)
f1 (&&):: (ty -> tr)
reemplazando 1 y 2 en 3
f1 (&&)::(Bool -> (Bool -> Bool))(3)
-}
Sea 
g x y z w = ((x*10) > (y*z)) && w
--deducir el tipo de las siguientes expresiones
--1)g::
--2) f1 g::
--3) f2 g::
--4) f1 g 8::
--5) f2 g 8::
--6) f1 g True::
--7) f2 g True::

ftotal:: tg -> tx -> ty -> tz -> tw -> tr
f1 g:: tx -> tg -> tr
f2 g:: ty -> tz -> tr
f1 g 8:: ((tx -> tg) -> (ty -> tz))
f2 g 8:: tx -> tr
f1 g True:: Bool -> Bool -> Bool
f2 g True:: 