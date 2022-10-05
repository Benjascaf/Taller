{-Repensar la funci ́on pertenece utilizando pattern matching.
Ejercicios
Resolver primero sin y despu ́es con pattern matching sobre listas
▶ productoria :: [Int] -> Int que devuelve la productoria de los elementos de una lista.
▶ sumarN :: Int -> [Int] -> [Int] que dado un n ́umero N y una lista xs, suma N a cada
elemento de xs.
▶ sumarElPrimero :: [Int] -> [Int] que dada una lista no vac ́ıa xs, suma el primer
elemento a cada elemento de xs. Ejemplo: sumarElPrimero [1,2,3] ⇝ [2,3,4]
▶ sumarElUltimo :: [Int] -> [Int] que dada una lista no vac ́ıa xs, suma el  ́ultimo
elemento a cada elemento de xs. Ejemplo: sumarElUltimo [1,2,3] ⇝ [4,5,6]
▶ pares :: [Int] -> [Int] que devuelve una lista con los elementos pares de una lista
dada. Ejemplo: pares [1,2,3,5,8] ⇝ [2,8]
▶ multiplosDeN :: Int -> [Int] -> [Int] que dado un n ́umero N y una lista xs, devuelve
una lista con los elementos m ́ultiplos N de xs.
▶ reverso :: [Int] -> [Int] que dada una lista invierte su orden.
▶ maximo :: [Int] -> Int que calcula el m ́aximo elemento de una lista no vac ́ıa.
▶ ordenar :: [Int] -> [Int] que ordena los elementos de una lista de forma creciente.
▶ quitar :: Int -> [Int] -> [Int] que elimina la primera aparici ́on del elemento en la
lista (de haberla).
▶ hayRepetidos :: [Int] -> Bool que indica si una lista tiene elementos repetidos.
▶ eliminarRepetidos :: [Int] -> [Int] que deja en la lista una  ́unica aparici ́on de cada
elemento, eliminando las repeticiones adicionales.
-}

longitud :: [a] -> Int 

longitud [] = 0
longitud ( _ : nums) = 1 + longitud nums


pertenece _ [] = False 
pertenece a (ai:as) = a == ai || pertenece a as
                    
productoria :: [Int] -> Int 
productoria nums | nums == [] = 1
                |otherwise = head nums * productoria (tail nums)

productoriap :: [Int] -> Int 
productoriap [] = 0 
productoriap (n : nums) = n * productoria nums

sumarN :: Int -> [Int] -> [Int] 
sumarN n nums | nums == [] = []
                |otherwise = (n + head nums) : sumarN n (tail nums)

sumarNp :: Int -> [Int] -> [Int]
sumarNp _ [] = [] 
sumarNp n (m:nums) = (n + m) : sumarNp n nums

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero nums = sumarN (head nums) nums

sumarElPrimerop :: [Int] -> [Int] 
sumarElPrimerop (n : nums) = sumarN n (n:nums)

sumarElUltimo :: [Int] -> [Int] 
sumarElUltimo nums = sumarN (encontrarUltimop nums) nums

encontrarUltimo :: [Int] -> Int 
encontrarUltimo nums  | longitud nums == 1 = head nums 
                      | otherwise = encontrarUltimo (tail nums)

encontrarUltimop [n] = n 
encontrarUltimop (n : nums) = encontrarUltimop nums

pares :: [Int] -> [Int] 
pares nums | nums == [] = []
            | mod (head nums) 2 == 0 = (head nums) : pares (tail nums)
            | otherwise = pares (tail nums)

paresp :: [Int] -> [Int] 
paresp [] = []
paresp (n:nums)  |mod n 2 == 0 = n : paresp nums 
                | otherwise = paresp nums

multiplosDeN :: Int -> [Int] -> [Int] 
multiplosDeN n nums | nums == [] = [] 
                    | mod (head nums) n == 0 = (head nums) : multiplosDeN n (tail nums) 
                    | otherwise = multiplosDeN n (tail nums)

multiplosDeNp :: Int -> [Int] -> [Int] 
multiplosDeNp _ [] = []
multiplosDeNp n (m : nums) | mod m n == 0 = m : multiplosDeNp n nums 
                            | otherwise = multiplosDeNp n nums

reverso :: [Int] -> [Int]
reverso nums | nums == [] = []
            | otherwise = (reverso (tail nums)) ++ [head nums]

reversop :: [Int] -> [Int] 
reversop [] = []
reversop (n: nums) = reverso nums ++ [n]

maximo :: [Int] -> Int 
maximo nums = maximoAuxP (head nums) nums 

maximoAux :: Int -> [Int] -> Int 
maximoAux n nums | nums == [] = n 
                | n < head nums = maximoAux (head nums) (tail nums)
                |otherwise = maximoAux n (tail nums)

maximoAuxP :: Int -> [Int] -> Int 
maximoAuxP n [] = n 
maximoAuxP n (m : nums) | n < m = maximoAuxP m nums 
                        |otherwise = maximoAuxP n nums
