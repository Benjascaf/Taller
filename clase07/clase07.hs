type Set a = [a] 

vacio :: Set Int
vacio = [] 

agregar :: Int -> Set Int -> Set Int 
agregar n a | elem n a = a 
            | otherwise = n : a 

incluido :: Set Int -> Set Int -> Bool  
incluido (a:as) b | as == [] = True 
                | not (elem a b) = False 
                | otherwise = incluido as b 

iguales :: Set Int -> Set Int -> Bool 
iguales a b = incluido a b && incluido b a 
   
partes :: Int -> Set (Set Int)
partes 0 = [[]]     
partes n =  agregarPartes n (partes (n-1)) ++ (partes (n -1))

agregarPartes :: Int -> Set (Set Int) -> Set (Set Int)
agregarPartes n [[]] = [agregar n vacio]
agregarPartes n (ls:lss) = (agregar n ls) : agregarPartes n lss

productoCartesiano :: Set Int -> Set Int -> Set (Int, Int) 

productoCartesiano [] b = [] 
productoCartesiano (a:as) b = formarDupla a b ++ productoCartesiano as b

formarDupla :: Int -> Set Int -> Set (Int, Int)
formarDupla a [] = [] 
formarDupla a (b:bs) = (a, b) : (formarDupla a bs)
