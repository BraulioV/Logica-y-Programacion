module Descifrado (descifrar) where

    import Data.Matrix      -- install with cabal install Matrix
    import Data.Vector

    eliminarEspacios :: String -> String
    eliminarEspacios texto = Prelude.filter (/= ' ') texto

    unirStrings :: [String] -> String
    unirStrings xs = Prelude.concat xs

    partirMasResto :: String -> Int -> Matrix Char
    partirMasResto texto n = transpose (Data.Matrix.fromList n a texto)
        where
            a = (Prelude.length texto) `div` n

    toLista :: Matrix Char -> [[Char]]
    toLista m = toListaAux m (ncols m) []
        where 
            toListaAux :: Matrix Char -> Int -> [[Char]] -> [[Char]]
            toListaAux _ 0 l = l
            toListaAux m n l = toListaAux m (n-1) (ml:l)
                where
                    ml = Data.Vector.toList (getCol n m)

    columnasConEspacios' :: [[Char]] -> [Int] -> [Int]
    columnasConEspacios' xs c = reverse' (columnasConEspaciosAux xs c [] 0)
        where
            columnasConEspaciosAux :: [[Char]] -> [Int] -> [Int] -> Int -> [Int]
            columnasConEspaciosAux [] _ r _ = r
            columnasConEspaciosAux (x:xs) c r n
                | Prelude.last x == ' ' = columnasConEspaciosAux xs c ((c !! e):r) (n+1)
                | otherwise             = columnasConEspaciosAux xs c r (n+1)
                    where
                        e = (c !! n)-1

    transponerLista :: Matrix Char -> [Int] -> String
    transponerLista m l = Prelude.concat (toLista (Data.Matrix.fromLists (shuffle (toLista m) l)))

    transponerListaFinal :: Matrix Char -> [Int] -> String
    transponerListaFinal m l = Prelude.concat (toLista (Data.Matrix.fromLists (shuffleFinal (toLista m) l)))

    shuffle :: [[Char]] -> [Int] -> [[Char]]
    shuffle m l = reverse' (shuffleAux m l [])
        where
            shuffleAux :: [[Char]] -> [Int] -> [[Char]] -> [[Char]]
            shuffleAux m ls r 
                | Prelude.length m == Prelude.length r = r
                | otherwise                            = shuffleAux m lst ((m !! i):r)
                    where
                        i   = Descifrado.elemIndex min ls
                        min = Prelude.minimum ls
                        max = Prelude.maximum ls
                        lst = Prelude.map (\x -> if x == min then x+max else x) ls

    shuffleFinal :: [[Char]] -> [Int] -> [[Char]]
    shuffleFinal m l = reverse' (shuffleFinalAux m l [])
        where
            shuffleFinalAux :: [[Char]] -> [Int] -> [[Char]] -> [[Char]]
            shuffleFinalAux _ [] r     = r
            shuffleFinalAux m (l:ls) r = shuffleFinalAux m ls ((m !! (l-1)):r)

    elemIndex :: Int -> [Int] -> Int
    elemIndex x xs  = elemIndexAux x xs 0
        where
            elemIndexAux :: Int -> [Int] -> Int -> Int
            elemIndexAux _ [] _  = error "La lista vacia no tiene elementos"
            elemIndexAux x (y:xs) n
                | x == y    = n
                | otherwise = elemIndexAux x xs (n+1)
    
    reverse' :: [a] -> [a]
    reverse' xs = reverseAux xs []
      where
        reverseAux :: [a] -> [a] -> [a]
        reverseAux [] lst     = lst
        reverseAux (x:xs) lst = reverseAux xs (x:lst)

    addEspacios :: String -> [Int] -> Int -> String
    addEspacios xs [] _     = xs
    addEspacios xs (l:ls) a = addEspacios (insert (l*a) ' ' xs) ls a

    insert :: Int -> Char -> String -> String
    insert 0 y xs = y:xs
    insert n y [] = [y]
    insert n y xs
        | Prelude.length xs < n = xs
        | otherwise = (Prelude.take (n-1) xs) Prelude.++ [y] Prelude.++ (Prelude.drop (n-1) xs)

    descifrar :: [String] -> [Int] -> String
    descifrar texto clave = fase4
        where
            fase1   = partirMasResto (unirStrings texto) (Prelude.length clave)
            fase1_5 = columnasConEspacios' (toLista fase1) clave
            fase2   = transponerLista fase1 clave
            fase3   = addEspacios (eliminarEspacios fase2) fase1_5 a
            fase4   = eliminarEspacios (transponerListaFinal (partirMasResto fase3  (Prelude.length clave)) clave)
            a = (Prelude.length fase2) `div` (Prelude.length clave)
