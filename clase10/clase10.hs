emcd :: Int -> Int -> (Int, Int ,Int) 
emcd a 0 = (a, 1, 0)
emcd a b =  (g, s, t)
        where
            (g, s' , t') = emcd b (mod a b) 
            t = s'- (t' * q)
            s = t'
            q = div a b 



fst3 (x, _, _) = x


tieneSolucion :: Int -> Int -> Int -> Bool  
tieneSolucion a b m = mod  b (fst3 (emcd a m )) == 0

solucionParticular :: Int -> Int -> Int -> Int
solucionParticular a b m 
                            | not (tieneSolucion a b m) = undefined
                            | b == mcd = x
                            | otherwise = x * c 
                        where 
                            (mcd, x, q) = emcd a m
                            c = div b mcd 

solucionGeneral :: Int -> Int -> Int -> (Int, Int)
solucionGeneral a b m 
                        | not (tieneSolucion a b m) = undefined 
                        | otherwise = (solucion, m')
                        where 
                            solucion = solucionParticular a' b' m' 
                            (a',b',m') = (div a mcd, div b mcd, div m mcd)
                            (mcd, _, _) = emcd a m