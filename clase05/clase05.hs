sumaDivisoresHasta:: Int -> Int -> Int
sumaDivisoresHasta q d 
                        | d == 1 = 1 
                        | mod q d == 0 = q + sumaDivisoresHasta q (d - 1)
                        | otherwise = sumaDivisoresHasta q (d - 1)

sumaDivisores:: Int -> Int 
sumaDivisores q = sumaDivisoresHasta q q

menorDivisor :: Int -> Int 
menorDivisor n = menorDivisorAux n 2

menorDivisorAux :: Int -> Int -> Int
menorDivisorAux n d | mod n d == 0 = d 
                    | otherwise = menorDivisorAux n (d + 1)

esPrimo :: Int -> Bool
esPrimo 0 = False
esPrimo 1 = False
esPrimo n = n == menorDivisor n

nEsimoPrimo :: Int -> Int 
nEsimoPrimo n = nEsimoPrimoAux n 0

nEsimoPrimoAux:: Int -> Int -> Int 
nEsimoPrimoAux n p | n == 0 = p - 1
                | esPrimo p = nEsimoPrimoAux (n-1) (p + 1)
                |otherwise = nEsimoPrimoAux n (p + 1)

{-Implementar menorFactDesde :: Int -> Int que dado m ≥ 1 encuentra el minimo n ≥ m
tal que n = k! para algun k -}

factorial :: Int -> Int 
factorial n | n == 0 = 1
            | otherwise = n * factorial (n - 1)

menorFactDesde :: Int -> Int 
menorFactDesde m = menorFactAux m 1


menorFactAux :: Int -> Int -> Int 
menorFactAux n f | n <= factorial f = factorial f 
                | otherwise = menorFactAux n (f + 1)

{-Implementar mayorFactHasta :: Int -> Int que dado m ≥ 1 encuentra el m ́aximo
n ≤ m tal que n = k! para alg ́un k-}

mayorFactHasta :: Int -> Int 
mayorFactHasta m = mayorFactHastaAux m 1

mayorFactHastaAux :: Int -> Int -> Int
mayorFactHastaAux m k | m < factorial k = factorial (k - 1)
                        |otherwise = mayorFactHastaAux m (k + 1)

{-mplementar esFact :: Int -> Bool que dado n ≥ 0 decide si existe un n ́umero entero
k ≥ 0 tal que n = k!-}

esFact :: Int -> Bool 
esFact n = esFactAux n 0

esFactAux :: Int -> Int -> Bool
esFactAux n k | n == factorial k = True 
                | n < factorial k = False 
                | otherwise = esFactAux n (k + 1)


esFibonacci :: Int -> Bool 
esFibonacci n = esFibonacciAux n 0

esFibonacciAux :: Int -> Int -> Bool 
esFibonacciAux n f | n == fibonacci f = True 
                    | n < fibonacci f = False 
                    | otherwise = esFibonacciAux n (f + 1)

fibonacci  :: Int -> Int 
fibonacci n | n == 0 = 0
            | n == 1 = 1 + fibonacci (n - 1)
            |otherwise = fibonacci(n - 1) + fibonacci (n - 2)

{-mplementar esSumaInicialDePrimos :: Int -> Bool que dado un n ́umero entero n ≥ 0
decide si n es igual a la suma de los m primeros n ́umeros primos, para alg ́un m-}

esSumaInicialDePrimos :: Int -> Bool 
esSumaInicialDePrimos n = esSumaInicialDePrimosAux n 1

esSumaInicialDePrimosAux :: Int -> Int -> Bool 
esSumaInicialDePrimosAux n sp | n == sumaPrimos sp = True 
                                |n < sumaPrimos sp = False 
                                |otherwise = esSumaInicialDePrimosAux n (sp + 1)

sumaPrimos :: Int -> Int 
sumaPrimos n | n == 1 = nEsimoPrimo n
             | otherwise = nEsimoPrimo n + sumaPrimos (n - 1)