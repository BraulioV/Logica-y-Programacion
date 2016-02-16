module SistemaCongruencias (module SistemaCongruencias) where

    import Congruencia

    class SistemasConNormalizacion b where
        normSistema :: b -> b
{-
    Sistema (Congruencia x y m) (Congruencia x' y' m')
-}
    data Sistema a = Sistema
        { ecuacion1 :: Congruencia a 
        , ecuacion2 :: Congruencia a 
        } deriving(Show)

    instance (Integral a) => SistemasConNormalizacion (Sistema a) where
        normSistema (Sistema (Congruencia a1 a2 a3) (Congruencia b1 b2 b3))
            | d /= 1    = error "Sistema sin solucion"
            | otherwise = Sistema (norm (Congruencia a1 a2 a3)) (norm (Congruencia b1 b2 b3))
            where 
                (d,s,_) = xeuclides a3 b3
