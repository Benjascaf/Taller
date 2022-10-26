import Clase07

combinatorio :: Integer -> Integer -> Integer 
combinatorio n 0 = 1 
combinatorio n m | m == n = 1 
                | otherwise = combinatorio (n-1) m + combinatorio (n - 1) (m - 1)

factorial :: Integer -> Integer 
factorial 0 = 1 
factorial n = n * (factorial (n - 1))
combinatoriop :: Integer -> Integer -> Integer 
combinatoriop n m = div (factorial n)  ((factorial (n - m)) * (factorial m))

variaciones :: Set Integer -> Integer -> Set [Integer]
variaciones s 0 = vacio 
variaciones [] l = vacio 
variaciones s l = agregarATodasListas s (variaciones s (l - 1))

agregarALista :: Integer -> Set [Integer] -> Set [Integer]
agregarALista n [] = vacio
agregarALista n (s : xs)  =  [n : s] ++ (agregarALista n xs)

agregarATodasListas :: Set Integer -> Set [Integer] -> Set [Integer]
agregarATodasListas [] ss = vacio 
agregarATodasListas (s : xs) ss = (agregarALista s ss) ++ (agregarATodasListas xs ss)

insertarEn :: [Integer] -> Integer -> Integer -> [Integer]
insertarEn [] n _ = [n]
insertarEn (l : ls) n i | i == 1 = n : (l:ls)
                        |otherwise = l : (insertarEn ls n (i-1))

permutaciones :: Integer -> Set [Integer]
permutaciones 1 = [[1]] 
permutaciones n = posiblesInsertacionesEnTodasLasListas (permutaciones (n - 1)) n

posiblesInsertaciones :: Integer -> [Integer] -> Integer -> Set [Integer]
posiblesInsertaciones n l 1 = [n : l]
posiblesInsertaciones n l i = [(insertarEn l n i)] ++ posiblesInsertaciones n l (i - 1)

posiblesInsertacionesEnTodasLasListas :: Set [Integer] -> Integer -> Set [Integer]
posiblesInsertacionesEnTodasLasListas [] n = vacio 
posiblesInsertacionesEnTodasLasListas (x:xs) n = posiblesInsertaciones n x n ++ posiblesInsertacionesEnTodasLasListas xs n


subconjuntos :: Integer -> Integer -> Set (Set Integer)
subconjuntos _ 0 = vacio 
subconjuntos 0 _ = vacio 
subconjuntos n k | n == k = [[1..n]]
                | otherwise = (agregarPartes n (subconjuntos (n-1) (k - 1))) ++ subconjuntos (n-1) k