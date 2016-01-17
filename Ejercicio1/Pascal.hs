-- version con redes de procesos
triangulo :: [[Int]]
triangulo = [1]:map f triangulo
    where
        f cs = zipWith (+) (0:cs) (reverse' (0:cs))

-- version que devuelve una fila determinada del triangulo
calcular_triangulo :: Int -> [Int]
calcular_triangulo 0 = [1]
calcular_triangulo n = zipWith (+) a b
    where
        a = 0:(calcular_triangulo (n-1))
        b = reverse' a
        