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

todoMenor :: (Ord t) => (t, t) -> (t, t) -> Bool
todoMenor (vx, vy) (wx, wy) = vx < wx && vy < wy