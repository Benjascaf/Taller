absoluto :: Int -> Int
absoluto x | x >= 0 = x
            | otherwise = -x

maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto x y | (absoluto x) >= (absoluto y) = x
                    | otherwise = y

-- No hacen faltan parentesisis para las operaciones condicionales
maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z | (x >= y && x >= z) = x
                | (y > x && y > z) = y
                | (z > x && z > y) = z

algunoEs0 :: Float -> Float -> Bool
algunoEs0 x y | x == 0 || y == 0 = True
                |otherwise = False

-- Pattern matching (PM)
algunoEs0PM :: Float -> Float -> Bool 
algunoEs0PM 0 _ = True
algunoEs0PM _ 0 = True 
algunoEs0PM x y = False

ambosSon0 :: Float -> Float -> Bool 
ambosSon0 x y | x == 0 && y == 0 = True
                |otherwise = False

-- Pattern matching
ambosSon0PM :: Float -> Float -> Bool
ambosSon0PM 0 0 = True
ambosSon0PM _ _ = False

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = mod y x == 0

digitoUnidades :: Int -> Int 
digitoUnidades x = mod x 10

digitoDecenas :: Int -> Int
digitoDecenas x = div (mod x 100) 10

digitoUnidades3 n = div (n - (div n 100) * 100) 10