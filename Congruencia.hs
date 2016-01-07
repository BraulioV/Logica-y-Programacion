module Congruencia (module Congruencia) where

    euclides :: Int -> Int -> Int
    euclides n 0 = n * signum n
    euclides m n = euclides n (m `mod` n)

    xeuclidesAux :: Int -> Int -> (Int, Int, Int)
    xeuclidesAux a 0  = (a, 1, 0)
    xeuclidesAux a b = (d,s,t-(a `div` b)*s)
                     where
                          (d,t,s) = xeuclidesAux b (a `mod` b )

    xeuclides :: Int -> Int -> (Int, Int, Int)
    xeuclides a b = (d,s*(signum a),t*(signum b))
                    where
                         (d,s,t) = xeuclidesAux (abs a) (abs b)

    class TiposConNormalizacion b where
        norm :: b -> b

    data Cong a = Cong (Int, Int, Int)

    instance (Integral a) => TiposConNormalizacion (Cong a) where
        norm (Cong (a, b, m)) 
            | (b `mod` d) /= 0 = error "Congruencia sin solucion"
            | m < 1            = norm (Cong (a, b, m*(signum m)))
            | otherwise        = Cong (1, b_2, m_2)
                where 
                    a_1 = a `div` d
                    b_1 = b `div` d
                    (d, t, _) = xeuclides a m
                    m_2 = m `div` d
                    b_2 = (b_1 * t) `mod` m_2

    instance Show a => Show (Cong a) where
        showsPrec _ (Cong (a, b, m)) = shows a.showString " := ".shows b.showString " (mod ".shows m.showChar ')'

    
                    