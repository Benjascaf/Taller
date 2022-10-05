{-Escribir una expresi ́on que denote la lista estrictamente decreciente de enteros que comienza
con el n ́umero 1 y termina con el n ́umero -100.-}

a = [1,0..(-100)]
b = [-19,-15..17]

sumatoria :: [Int] -> Int 
sumatoria l     | l == [] = 0
                |otherwise = head l + sumatoria (tail l)

longitud :: [Int] -> Int 
longitud nums | nums == [] = 0
                |otherwise = 1 + longitud (tail nums)

pertenece :: Int -> [Int] -> Bool 
pertenece n nums | nums == [] = False 
                | n == head nums = True 
                |otherwise = pertenece n (tail nums)

{-Usando pattern matching-}
sumatoria2 [] = 0
sumatoria2 (x:xs) = sumatoria xs + x    

longitud2 :: [a] -> Int 

longitud2 [] = 0
longitud2 ( _ : nums) = 1 + longitud2 nums