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

    transponerListaFinal :: Matrix Char -> [Int] -> String
    transponerListaFinal m l = Prelude.concat (shuffleFinal (toLista m) l)

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
            shuffleAux m ls r 
                | Prelude.length m == Prelude.length r = r
                | otherwise                            = shuffleAux m lst ((m !! i):r)
                    where
                        i   = Cifrado.elemIndex min ls
                        min = Prelude.minimum ls
                        max = Prelude.maximum ls
                        lst = Prelude.map (\x -> if x == min then x+max else x) ls

    shuffleFinal :: [[Char]] -> [Int] -> [[Char]]
    shuffleFinal m l = reverse' (shuffleFinalAux m l [])
        where
            shuffleFinalAux :: [[Char]] -> [Int] -> [[Char]] -> [[Char]]
            shuffleFinalAux _ [] r     = r
            shuffleFinalAux m (l:ls) r = shuffleFinalAux m ls ((m !! (l-1)):r)

    reverse' :: [a] -> [a]
    reverse' xs = reverseAux xs []
      where
        reverseAux :: [a] -> [a] -> [a]
        reverseAux [] lst     = lst
        reverseAux (x:xs) lst = reverseAux xs (x:lst)

    elemIndex :: Int -> [Int] -> Int
    elemIndex x xs  = elemIndexAux x xs 0
        where
            elemIndexAux :: Int -> [Int] -> Int -> Int
            elemIndexAux _ [] _  = error "La lista vacia no tiene elementos"
            elemIndexAux x (y:xs) n
                | x == y    = n
                | otherwise = elemIndexAux x xs (n+1)
    
    cifrar :: String -> [Int] -> [String]
    cifrar texto clave = reverse' (dividir fase4 5)
        where
            fase1 = partirMasResto (eliminarEspacios texto) (Prelude.length clave)
            fase2 = transponerLista fase1 clave
            fase3 = partirMasResto (eliminarEspacios fase2) (Prelude.length clave)
            fase4 = transponerListaFinal fase3 clave

    dividir :: [Char] -> Int -> [[Char]]
    dividir texto div = dividirAux texto div []
        where
            dividirAux :: [Char] -> Int -> [[Char]] -> [[Char]]
            dividirAux [] _ r      = r
            dividirAux texto div r = dividirAux (Prelude.drop div texto) div ((Prelude.take div texto):r)
