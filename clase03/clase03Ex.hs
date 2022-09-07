{-
mplementar la funci ́on fib : Z≥0 → Z que devuelve el i- ́esimo n ́umero de Fibonacci.
Recordar que la secuencia de Fibonacci se define como:
fib(n) =
0 si n = 0
1 si n = 1
fib(n − 1) + fib(n − 2) en otro caso-}

fib :: Int -> Int

fib 0 = 0
fib 1 = 1
fib i = fib(i - 1) + fib(i - 2)

{-implementar una funci ́on parteEntera :: Float -> Integer que calcule la parte entera
de un n ́umero real positivo-}

parteEntera :: Float -> Integer
parteEntera n |  n < 1 = 0
              | otherwise = 1 + parteEntera(n - 1)


{-M ́as funciones recursivas
Ejercicios
1 Escribir una funci ́on para determinar si un n ́umero natural es m ́ultiplo de 3. No est ́a
permitido utilizar mod ni div.-}

esMultiploDe3 :: Int -> Bool
esMultiploDe3 n | n == 0 = True
                | n < 0 = False
                | otherwise = esMultiploDe3 (n - 3)


{-implementar la funci ́on sumaImpares :: Int -> Int que dado n ∈ N sume los primeros n
n ́umeros impares-}

nEsimoImpar :: Int -> Int 
nEsimoImpar n | n <= 1 = 1
                | otherwise = 2 + nEsimoImpar(n - 1)


sumaImpares :: Int -> Int
sumaImpares n | n == 1 = 1
                |otherwise = nEsimoImpar(n) + sumaImpares(n - 1)

{-Escribir una funci ́on medioFact que dado n ∈ N calcula n!! = n (n − 2)(n − 4) · · · . Por
ejemplo:-}

medioFact :: Int -> Int 
medioFact 0 = 1
medioFact 1 = 1
medioFact n = n * medioFact (n - 2)

{-scribir una funci ́on que determine la suma de d ́ıgitos de un n ́umero positivo. Para esta
funci ́on pueden utilizar div y mod.-}

sumaDigitos :: Int -> Int 
sumaDigitos n | div n 10 == 0 = n
                |otherwise = (mod n 10)  + sumaDigitos(div n 10)