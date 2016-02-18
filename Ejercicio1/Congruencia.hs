module Congruencia (module Congruencia) where

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

    -- definimos el tipo de dato Congruencia
    data Congruencia a = Congruencia 
        { a :: Int
        , b :: Int
        , modul :: Int
        }

    -- para poder mostrar una congruencia, debemos incluirla en la clase Show
    instance Show a => Show (Congruencia a) where
        showsPrec _ (Congruencia x y m) = shows x.showString " := ".shows y.showString " (mod ".shows m.showChar ')'

    -- definimos la normalizacion de una congruencia
    {-
        Esta funcion respondera con la solucion a la congruencia
        ax=b(mod m), si existe se respondera con el par (b',m') y
        la solucion sera de la forma x=b'(mod m'). Si no tiene solucion
        se respondera con el par (0,0)

        Ejemplo.
        195x = 549 (mod 294)    -> MCD (195, 294) = 3
        65x  = 183 (mod 98)     -> 183^-1 (mod 98) = 183*(-3) (mod 98) <- Bezout
                                -> 65*(-3) (mod 98) = 1
        x    = 39 (mod 98)
    -}
    instance Integral a => TiposConNormalizacion (Congruencia a) where
        norm (Congruencia x y m)
            | y `mod` d /= 0 = error "Congruencia sin solucion"
            | m < 1          = norm (Congruencia x y ((signum m)*m))
            | otherwise      = Congruencia 1 e f
            where
                (d,s,_) = xeuclides x m
                h = y `div` d
                e = (h * s) `mod` f
                f = m `div` d

    -- definimos la igualdad de congruencias
    instance (Eq a, Num a) => Eq (Congruencia a) where
        (Congruencia a b m) == (Congruencia a' b' m') = (c == c') && (d == d') && (e == e')
            where
                Congruencia c d e     = norm (Congruencia a b m)
                Congruencia c' d' e'  = norm (Congruencia a' b' m')