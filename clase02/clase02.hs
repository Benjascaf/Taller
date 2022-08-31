identidad :: t -> t
identidad x = x 

primero :: tx -> ty -> tx
primero x y = x

segundo:: tx -> ty -> ty
segundo x y = y

constante5 :: tx -> ty -> tz -> Int
constante5 x y z = 5    

mismoTipo :: t-> t-> Bool
mismoTipo x y = True

suma x y = x + y 

triple x = 3*x

maximo x y | x >= y = x
            |otherwise = y

distintos x y = x /= y
 
-- _____________________________________________________________________________________________________________________________________________________________--

f1 x y z = x**y + z <= x + y ** z

f2 x y = (sqrt x) / (sqrt y)

f3 x y = div (sqrt x) (sqrt y)

f4 x y z | x == y = z
        | x ** y ==  y = x
        |otherwise = y  

f5 x y z | x == y = z
        | x ** y == y = x
        |otherwise = z

-- _____________________________________________________________________________________________________________________________________________________________ --


-- su :: (Num t) => (t, t) -> (t, t) -> (t, t) --
su (vx, vy) (wx, wy) = (vx + wx, vy + wy)