{-
Algoritmo extendido de euclides

| q | a | b | u0 | u1 | v0 | v1
:..::..::..::...::...::...::...:
| - |50 |20 | 1  | 0  |  0 | 1 |
| 2 |20 |10 | 0  | 1  |  1 |-2 |
| 2 |10 | 0 | 1  | -  | -2 | - |

10 = 1*50 + (-2)*20 siendo q = a `div` b
mcd (a, b) = mcd (b, a%b) = mcd (b, b - q*a)
-}
xeuclides :: Int -> Int -> [Int]
xeuclides a b = [d,s,t]
                where
                    sa = signum a
                    sb = signum b 
                    va = abs a 
                    vb = abs b 
                    d  = (xeuclidesAux va vb) !! 0
                    s  = sa * (xeuclidesAux va vb) !! 1
                    t  = sb * (xeuclidesAux va vb) !! 2

xeuclidesAux :: Int -> Int -> [Int]
xeuclidesAux a 0 = [a, 1, 0]
xeuclidesAux a b = [d, m, n - (a `div` b) * m]
        where
            [d,n,m] = xeuclidesAux b (a `mod` b)

-- Algoritmo extendido de euclides con una red de procesos
xeuclides_red :: Int -> Int -> [(Int,Int,Int,Int,Int,Int,Int)]
xeuclides_red a b = (0,a,b,1,0,0,1):map e (xeuclides_red a b)
    where 
        e (c,a,b,u0,u1,v0,v1) = (q,b,r,u1,u1',v1,v1')
            where
                (q,r) = a `divMod` b
                u1'   = u0 - (q * u1)
                v1'   = v0 - (q * v1)

-- funcion para evitar que se produzca la excepcion de dividir por cero
xeuclides_redTW :: Int -> Int -> [(Int,Int,Int,Int,Int,Int,Int)]
xeuclides_redTW a b = takeWhile cd (xeuclides_red a b)
    where
        cd :: (Int,Int,Int,Int,Int,Int,Int) -> Bool
        cd (q,a,b,u0,u1,v0,v1)
            | b == 0    = False
            | otherwise = True

xeuclides_redIT :: Int -> Int -> (Int,Int,Int)
xeuclides_redIT 0 b = (b * signum b, 0, signum b)
xeuclides_redIT a 0 = (a * signum a, signum a, 0)
xeuclides_redIT a b = (d * signum d, signum d * u0, signum d * v0)
    where
        (q,a1,d,u0,u1,v0,v1) = last (xeuclides_redTW a b)