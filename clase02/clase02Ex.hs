{-estanRelacionados: dados dos n ́umeros reales, decide si est ́an relacionados considerando
la relaci ́on de equivalencia en R cuyas clases de equivalencia son (−∞, 3], (3, 7] y (7, ∞).-}

estanRelacionados :: (Ord tx, Ord ty, Num tx, Num ty) => tx -> ty -> Bool
estanRelacionados x y = (x <= 3 && y <= 3) || x > 7 && y > 7 || x > 3 && x <= 7 && y > 3 && y <=7


{-distanciaPuntos: calcula la distancia entre dos puntos de R2
.-}

distanciaPuntos :: (Floating t) => (t, t) -> (t, t) -> t
distanciaPuntos (vx, vy) (wx, wy) = sqrt ((vx - wx)^2 + (vy - wy)^2)


{-prodInt: calcula el producto interno entre dos vectores de R2
.-} 
prodInt :: (Num t) => (t, t) -> (t, t) -> t 
prodInt (vx, vy) (wx, wy) = vx * wx + vy * wy 


{-todoMenor: dados dos vectores de R2
, decide si es cierto que cada coordenada del primer
vector es menor a la coordenada correspondiente del segundo vector.-}

todoMenor :: (Ord t) =>  (t, t) -> (t, t) -> Bool
todoMenor (vx, vy) (wx, wy) = vx < wx && vy < wy

{-sumaTerna: dada una terna de enteros, calcula la suma de sus tres elementos.-}
sumaTerna :: (Integer, Integer, Integer) -> Integer 
sumaTerna (x, y, z) = x + y + z
{-posicPrimerPar: dada una terna de enteros, devuelve la posici´on del primer n´umero par si
es que hay alguno, y devuelve 4 si son todos impares.-}

posicPrimerPar :: (Integer, Integer, Integer) -> Integer
posicPrimerPar (x, y, z) | (mod x 2 == 0) = 1
                            |(mod y 2 == 0) = 2
                            | (mod z 2 == 0) = 3
                            | otherwise = 4

{-crearPar :: a -> b -> (a, b): crea un par a partir de sus dos componentes dadas por
separado (debe funcionar para elementos de cualquier tipo).-}

crearPar :: a -> b -> (a, b)
crearPar x y = (x, y)

{-invertir :: (a, b) -> (b, a): invierte los elementos del par pasado como par´ametro
(debe funcionar para elementos de cualquier tipo).-}

invertir :: (a,b) -> (b, a)
invertir (x, y) = (y, x)