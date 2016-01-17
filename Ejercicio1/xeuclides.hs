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
xeuclides_red :: Int -> Int -> Int -> Int -> Int -> Int -> [[Int]]
xeuclides_red a b u0 u1 v0 v1 = takeWhile ((a `mod` b) > 0) [q,b,m,u0',u1',v0',v1']:xeuclides_red b m u0' u1' v0' v1'
    where
        q   = a `div` b
        m   = a `mod` b
        u0' = u1
        u1' = u0 - (q * u1)
        v0' = v1
        v1' = v0 - (q * v1) 
