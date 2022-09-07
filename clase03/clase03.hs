factorial :: Integer -> Integer

factorial n | n == 0 = 1
              | otherwise = n * factorial (n - 1)

factorial2 :: Int -> Int 
factorial2 0 = 1
factorial2 n = n * (n - 1)


{-Funcion con problema-}

esPar :: Int -> Bool
esPar n | n == 0 = True
        |otherwise =  not (esPar (n - 1))