type Polinomio = [Float]

limpiar :: [Float] -> Polinomio
limpiar (n:ns) | not (n == 0) = (n:ns) 
                |otherwise = limpiar ns

limpiar2 :: [Float] -> Polinomio 
limpiar2 (0:xs) = limpiar2 xs 
limpiar2 xs = xs

grado :: Polinomio -> Int 
grado [] = undefined 
grado (x:xs) | xs == [] = 1 
            | otherwise = 1 + grado xs

evaluar :: Polinomio -> Float -> Float 
evaluar [] _ = 0
evaluar (x:xs) a | (xs) == [] = x 
                 | otherwise = x * (a ^ (grado (x:xs))) + evaluar xs a

suma :: Polinomio -> Polinomio -> Polinomio 
suma [] ys = ys
suma xs [] = xs 
suma xs ys = limpiar2 (suma (init xs) (init ys) ++ [(last xs) + (last ys)])

type Monomio = (Float, Int)

productoPorEscalar :: Float -> Polinomio -> Polinomio 
productoPorEscalar n [x] = [x*n]
productoPorEscalar n p = limpiar (productoPorEscalar n (init p) ++ [ n * (last p)])

resta :: Polinomio -> Polinomio -> Polinomio 
resta p q = suma p (productoPorEscalar (-1) q)

productoPorMonomio :: Monomio -> Polinomio -> Polinomio
productoPorMonomio (a,0) p = productoPorEscalar a p 
productoPorMonomio (a, n) p = (productoPorMonomio (a, n - 1) p )++ [0]

producto :: Polinomio -> Polinomio -> Polinomio 
producto [] p = p 
producto q [] = q
producto [x] p = productoPorMonomio (x, 0) p 
producto (q:qs) p = suma (producto (limpiar qs) p) (productoPorMonomio ( q, grado qs) p)

hacerPolinomio :: Monomio -> Polinomio 
hacerPolinomio (a, n) = productoPorMonomio (1, n) [a]

derivadaMonomio :: Monomio -> Monomio 
derivadaMonomio (a, 0) = undefined 
derivadaMonomio (a, n) = (a * (fromIntegral n), n - 1)

derivada :: Polinomio -> Polinomio
derivada [] = [] 
derivada [x] = [] 
derivada (p:ps) = fst (derivadaMonomio (p,grado ps)) : derivada ps

primerCociente :: Polinomio -> Polinomio -> Monomio 
primerCociente (p:ps) (q:qs) |  grado (p:ps) < grado (q:qs) = undefined 
                              |otherwise =  (p / q, grado (p:ps) - grado (q:qs))

primerResto :: Polinomio -> Polinomio -> Polinomio
primerResto p q | grado p < grado q = undefined 
                | otherwise = resta p  (productoPorMonomio (primerCociente(p, q) * q))
division :: Polinomio -> Polinomio -> (Polinomio, Polinomio)
division p q | grado p < grado q = ([], p) 
             | (suma (primerCociente p q) c', r')
             where 
                (c', r') = division (primerResto p q) q 

mcdp :: Polinomio -> Polinomio -> Polinomio 
mcdp p 0 = productoPorEscalar (1/head p) p 
mcdp p q = mcdp q (snd (division p q))

