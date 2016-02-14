module Cifrado (cifrar) where

    import Data.Matrix      -- install with cabal install Matrix
    import Data.Vector

    eliminarEspacios :: String -> String
    eliminarEspacios texto = Prelude.filter (/= ' ') texto

    partirMasResto :: String -> Int -> Matrix Char
    partirMasResto texto n
        | (Prelude.length texto) < n = error "longitud del mensaje menor a la clave"
        | b == 0             = Data.Matrix.fromList a n texto
        | otherwise          = Data.Matrix.fromList (a+1) n t
            where
                a = (Prelude.length texto) `div` n
                b = (Prelude.length texto) `mod` n
                t = addWhiteSpaces texto (n - b)

    addWhiteSpaces :: String -> Int -> String
    addWhiteSpaces texto 0 = texto
    addWhiteSpaces texto n = addWhiteSpaces (texto Prelude.++ " ") (n-1)

    transponerLista :: Matrix Char -> [Int] -> String
    transponerLista m l = Prelude.concat (shuffle (toLista m) l)

    toLista :: Matrix Char -> [[Char]]
    toLista m = toListaAux m (ncols m) []
        where 
            toListaAux :: Matrix Char -> Int -> [[Char]] -> [[Char]]
            toListaAux _ 0 l = l
            toListaAux m n l = toListaAux m (n-1) (ml:l)
                where
                    ml = Data.Vector.toList (getCol n m)

    shuffle :: [[Char]] -> [Int] -> [[Char]]
    shuffle m l = reverse' (shuffleAux m l [])
        where
            shuffleAux :: [[Char]] -> [Int] -> [[Char]] -> [[Char]]
            shuffleAux _ [] r     = r
            shuffleAux m (l:ls) r = shuffleAux m ls ((m !! (l-1)):r)
    
    reverse' :: [a] -> [a]
    reverse' xs = reverseAux xs []
      where
        reverseAux :: [a] -> [a] -> [a]
        reverseAux [] lst     = lst
        reverseAux (x:xs) lst = reverseAux xs (x:lst)

    cifrar :: String -> [Int] -> [[Char]]
    cifrar texto clave = reverse' (dividir fase4 5)
        where
            fase1   = partirMasResto (eliminarEspacios texto) (Prelude.length clave)
            fase2y3 = partirMasResto (eliminarEspacios (transponerLista fase1 clave)) (Prelude.length clave)
            fase4   = transponerLista (fase2y3) clave

    dividir :: [Char] -> Int -> [[Char]]
    dividir texto div = dividirAux texto div []
        where
            dividirAux :: [Char] -> Int -> [[Char]] -> [[Char]]
            dividirAux [] _ r      = r
            dividirAux texto div r = dividirAux (Prelude.drop div texto) div ((Prelude.take div texto):r)
