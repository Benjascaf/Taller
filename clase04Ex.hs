{-f 1(n) = n ∑i=0 2i , n ∈ N0.-}

f1 :: Int -> Int
f1 n | n == 0 = 1
        |otherwise =  2 ^ n + f1 (n - 1)

f2 :: Int -> Float -> Float
f2 n q |n == 1 = q 
        | otherwise = q ^ n + f2 (n - 1) q

f3 :: Int -> Float -> Float 
f3 n q =  q ^ (2 * n) +  f2 (2*n - 1) q 

f3independiente :: Int -> Float -> Float 
f3independiente n q | n == 1 = q + q ^ 2
                    |otherwise = f3independiente (n - 1) q + q ^ (2 * n - 1) + q ^ (2 * n)

f4 :: Int -> Float -> Float 
f4 n q  | n == 0 = 1
        | otherwise = f3independiente n q - f2 (n - 1) q

{-Ejercicios
I Implementar una funci ́on eAprox :: Integer -> Float que aproxime el valor del n ́umero
e a partir de la siguiente sumatoria:-}

fact :: Int -> Int 
fact n | n == 0 = 1
        |otherwise = n * fact (n - 1)

{-q es Infinity?-}
eAprox ::  Int -> Float 
eAprox n | n == 0 = 1
        | otherwise =  eAprox (n - 1) + (1 / (fromIntegral (fact n))) 

{-Definir la constante e :: Float como la aproximaci ́on de e a partir de los primeros 10
t ́erminos de la serie anterior.-}
e :: Float 
e = eAprox 10


{-
2 Implementar una funci ́on sumaPotencias q n m que sume todas las potencias de la forma
qa+b con 1 ≤ a ≤ n y 1 ≤ b ≤ m.-}

sumaPotencias :: Float -> Int -> Int -> Float 
sumaPotencias q n m | n == 1 = sumaPotenciaFija q n m 
                        | otherwise = sumaPotenciaFija q n m + sumaPotencias q (n - 1) m

sumaPotenciaFija :: Float -> Int -> Int -> Float 
sumaPotenciaFija q n m | m == 1 = q ** fromIntegral((n + 1))
                        | otherwise = q ** fromIntegral((n + m)) + sumaPotenciaFija q n (m - 1)


sumaPotenciaIndependiente :: Float -> Int -> Int -> Float
sumaPotenciaIndependiente q n m | m == 1 = q ^ 2
                                | otherwise =  q ^ (n + m) +  q ^ (n - 1) * sumaPotenciaIndependiente q 1 (m - 1)