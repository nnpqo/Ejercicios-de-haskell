--Escribir la definición de tipos de las siguientes funciones. Por ejemplo la
--definición de tipo de la función f x = 2*x es f::Int -> Int:
f1:: Bool -> Int -> Int -> Int
f1 x y z = if x 
            then y+10
            else z

f2::Int -> Int -> Char -> Int
f2 x y z = if z=='s' 
            then 2*x
            else y

f3:: (Int -> Bool) -> (Char -> Bool) -> (Bool -> Bool) -> Bool
f3 x y z = (x 2) && (y 'a') && (z True)


f4:: (Int -> Bool) -> a -> Int -> Int -> Int
f4=(\x -> \y -> \z -> \w -> if x 2 then z else w+10 )

f5::((a -> Int), a) -> ((b -> Int), b) -> Int
f5=(\(x,y) -> \(z,w) -> (x y)+ (z w))

f6:: a -> a
f6 x = x

--f7:: (y -> r) -> y -> r
f7:: (ty -> tr) -> ty -> tr
f7 x y = x y

--f8:: (t1->r)->(z->t1)->z->r
f8:: (ty -> tx) -> (tz -> ty) -> tz -> tx
f8 x y z = x (y z)

f9:: (a -> b -> c) -> a -> b -> c
f9 x y z = (x y) z

--f10:: (a->r) -> (b->a) -> (c->b) -> c -> r
f10:: (ty -> tr) -> (tz -> ty) -> (tw -> tz) -> tw -> tr
f10 x y z w= x (y (z w))

--f11:: (y -> r) -> (z -> r) -> z -> r
--f11 x y z w= ((x y) z) w

--f12:: (y -> r1 -> r) -> y -> (w -> r1) -> w -> r(investigar)
f12:: (ty -> tz -> tr) -> ty -> (tw -> tz) -> tw -> tr
f12 x y z w= ((x y) (z w))

f13::Int -> Int -> Int -> Int
f13 x y z = x*2+y

--f14:: (a->b->c)->a->b->c
f14:: (ty -> tz -> tx) -> ty -> tz -> tx
f14 x y z = x y z 

--fx:: Int -> Int -> Int
fx y z = if y > 3 then z else 4

--f15:: Bool -> Bool -> Bool -> Bool 
f15 x y z 
        | x = y
        | y = z

--f16:: (r1 -> r1 -> r) -> (z -> r1) -> z -> r
f16:: tx -> (tz -> ty) -> tz -> tr
f16 x y z = x (y z)(y z)

c17:: (t1 -> t2 -> r) -> (t1,t2) -> r
c17 f (x, y) = f x y

u18:: ((t1,t2) -> r) -> t1 -> t2 -> r
u18 f x y = f(x, y)

--p -> a -> (a -> a) -> a
f19:: x -> Int -> (Int -> Int) -> Int
f19 x y z = s + y
        where s = z y

f20:: (Int -> Int -> Int) -> Int -> Int -> Int 
f20 x y z = r1 + r2
        where r1 = x 5 y
              r2 = x y z

c21:: ((x,y) -> r) -> x -> y -> r
c21 f = g
        where g x y = f (x,y)

u22::(x -> y -> r) -> (x,y) -> r
u22 f = g
        where g (x,y) = f x y

c23:: ((x,y,z) -> r) -> x -> y -> z -> r
c23 f = g
        where g x y z = f(x, y, z)

f24:: Bool -> Bool -> (Bool -> Bool) -> Bool -> a -> Bool 
f24 e x y z w = if x && (y x) then z else w z
        where w a |a = e
                  |otherwise = z

f25::Int->Int->Int->Bool->Int->Bool->Int
f25 a b c d e f = a

f26::(Int->Int)->Int->Bool->Int->Bool->Int
f26 a b c d e = a b 

--fun:: (Char -> Int) -> ((Bool -> Int) -> Char) -> (Char -> Int)
--fun x y z 
  --          |x y = y
    --        |x y z = y
      --      |x z = z
          

--f::Int->Int->Int->Bool->Int->Bool->Int
fun2 x y = if x == "hola" then (*) else (+)
fun2:: String -> y -> (Int -> Int -> Int)
fun3 x y = (x 5) && (y 'a')
fun3:: (Int -> Bool) -> (Char -> Bool) -> Bool
fun4 x = x
fun4:: tx -> tx
fun5 x y = x y
fun5:: (ty -> tr) -> ty -> tr
fun6 x y z = x (y z)
fun6:: (ty -> tr) -> (tz -> ty) -> tz -> tr
fun7 x y z = (x y) z
--fun7:: tx -> ty -> tz -> tr
--fun7:: (ty -> tx) -> ty -> tz -> tx (investigar porque tx recibe 2 argumentos)
