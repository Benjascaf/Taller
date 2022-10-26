 module Clase07
 where
    type Set a = [a] 

    
    vacio = [] 

    agregar :: Integer -> Set Integer -> Set Integer 
    agregar n a | elem n a = a 
                | otherwise = n : a 

    incluido :: Set Integer -> Set Integer -> Bool  
    incluido (a:as) b | as == [] = True 
                    | not (elem a b) = False 
                    | otherwise = incluido as b 

    iguales :: Set Integer -> Set Integer -> Bool 
    iguales a b = incluido a b && incluido b a 
    
    partes :: Integer -> Set (Set Integer)
    partes 0 = [[]]     
    partes n =  agregarPartes n (partes (n-1)) ++ (partes (n -1))

    agregarPartes :: Integer -> Set (Set Integer) -> Set (Set Integer)
    agregarPartes n [] = [agregar n vacio]
    agregarPartes n [[]] = [agregar n vacio]
    agregarPartes n (ls:lss) = (agregar n ls) : agregarPartes n lss

    productoCartesiano :: Set Integer -> Set Integer -> Set (Integer, Integer) 

    productoCartesiano [] b = [] 
    productoCartesiano (a:as) b = formarDupla a b ++ productoCartesiano as b

    formarDupla :: Integer -> Set Integer -> Set (Integer, Integer)
    formarDupla a [] = [] 
    formarDupla a (b:bs) = (a, b) : (formarDupla a bs)
