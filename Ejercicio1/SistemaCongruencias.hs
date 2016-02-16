module SistemaCongruencias (module SistemaCongruencias) where

    import Congruencia

    class SistemasConNormalizacion b where
        normSistema :: b -> b
{-
    data Sistema a = Sistema (Congruencia (Int, Int, Int), Congruencia (Int, Int, Int))
-}
    data Sistema a = Sistema
        { ecuacion1 :: Congruencia (a, a, a)
        , ecuacion2 :: Congruencia (a, a, a)
        } deriving(Show)

    --instance (Integral a, Integral b) => SistemasConNormalizacion (Sistema(Congruencia x, Congruencia z)) where
    --    normSistema (Sistema (Congruencia(a1, a2, a3), Congruencia(b1, b2, b3)))
    --                | d /= 1         = error "Sistema sin solucion"
    --                | otherwise      = (Sistema((norm (Congruencia(a1, a2, a3))), (norm (Congruencia(b1, b2, b3)))))
    --                where
    --                    (d,s,_) = xeuclides a3 b3

        
    --        --    cong2        = norm (Congruencia(b1, b2, b3))
    
