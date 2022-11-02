division :: Int -> Int -> (Int, Int) 
division n 0 = (0, 0)
division n m |m < n && m > 0 = (0, m)
             |m < 0 = (-fst(q) - 1, -snd(q) + n)
             | otherwise = (fst d + 1, snd d)
             where 
                d = division n (m - n)
                q = division n (-m)

mcd :: Int -> Int -> Int 
mcd a b | b == 0 = a
        | otherwise = mcd b (snd (division b a))

menorDivisor :: Int -> Int 
menorDivisor n = menorDivisorAux n 2

menorDivisorAux :: Int -> Int -> Int
menorDivisorAux n d | mod n d == 0 = d 
                    | otherwise = menorDivisorAux n (d + 1)

mcd2 :: Int -> Int -> Int 
mcd2 1 _ = 1
mcd2 _ 1 = 1
mcd2 a b | mda == mdb = mdb * mcd2 (div a (mdb)) (div b (mdb))
        | mda < mdb = mcd2 (div a (mda)) b 
        |otherwise = mcd2 a (div b (mdb))
        where
            mda = menorDivisor a
            mdb = menorDivisor b

fst3 (x, _, _) = x
snd3 (_, y, _) = y
trd3 (_,_, z) = z

emcd :: Int -> Int -> (Int, Int ,Int) 
emcd a b | emcd a 0 = (a, 1, 0)
        |otherwise = emcd (g, s, t)
        where
            (g, s' , t') = emcd b (mod a b) 
            t = (s'- t' * q)
            s = t'
            q = div a b 


