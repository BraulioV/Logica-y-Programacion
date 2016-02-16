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
            | a3 `mod` d /= 0  = error "Sistema sin solucion"
            | otherwise        = Sistema (norm (Congruencia a1 a2 a3)) (norm (Congruencia b1 b2 b3))
            where 
                (d,s,_) = xeuclides a3 b3


    solve :: (Integral a) => (Sistema a) -> Int -> Int -> (Int, Int)
    solve sistema lim_inf lim_sup = (li, lsup)
        where
            normalizado = normSistema sistema
            ec1         = ecuacion1 normalizado
            ec2         = ecuacion2 normalizado
            k1          = norm (Congruencia (modul ec1) ((b ec2) - (b ec1)) (modul ec2))
            k           = norm (Congruencia 1 ((b ec2) + ((modul ec1)*(b k1))) ((modul ec1)*(modul k1)))
            li          = round ((fromIntegral (lim_inf - (a k)))/(fromIntegral(modul k))) --, (fromIntegral(lim_sup - (a k)))/(modul k))
            lsup        = round ((fromIntegral(lim_sup - (a k)))/(fromIntegral(modul k)))
