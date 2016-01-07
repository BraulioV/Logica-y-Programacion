import Congruencia
import Data.List

alfabeto = " abcdefghijklmnopqrstuvwxyz0123456789,.;:"

-- cifraAfin ::  a      b     Mensaje  Resultado
cifraAfinGen :: Int -> Int -> String -> String
cifraAfinGen a b m = reverse cifraAux a b m (length alfabeto) []
    where
        cifraAux :: Int -> Int -> String -> Int -> String -> String
        cifraAux a b [] l codificado      = codificado
        cifraAux a b (x:msg) l codificado = cifraAux a b msg l (alfabeto !! ((a*head (elemIndices x alfabeto) + b) `mod` l):codificado)

llaveDescifradoAfinGen :: Int -> Int -> (Int, Int)
llaveDescifradoAfinGen a b = (key_a, key_b)
    where
        (_, key_a, _) = xeuclides a (length alfabeto)
        key_b = (-key_a * b) `mod` (length alfabeto)