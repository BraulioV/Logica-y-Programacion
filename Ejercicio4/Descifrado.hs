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

    columnasConEspacios :: [[Char]] -> [Bool]
    columnasConEspacios xs = Prelude.map (\x -> Prelude.last x == ' ') xs

    transponerLista :: Matrix Char -> [Int] -> String
    transponerLista m l = Prelude.concat (toLista (Data.Matrix.fromLists (shuffle (toLista m) l)))

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

    addEspacios :: String -> [Bool] -> Int -> String
    addEspacios xs lst a = addEspaciosAux xs lst a 1
        where
            addEspaciosAux :: String -> [Bool] -> Int -> Int -> String
            addEspaciosAux xs [] _ _ = xs
            addEspaciosAux xs (l:lst) a tam
                | l == True = addEspaciosAux (insert (tam*a) ' ' xs) lst a (tam+1)
                | otherwise = addEspaciosAux xs lst a (tam+1)

    insert :: Int -> Char -> String -> String
    insert 0 y xs = y:xs
    insert n y [] = [y]
    insert n y xs
        | Prelude.length xs < n = xs
        | otherwise = (Prelude.take (n-1) xs) Prelude.++ [y] Prelude.++ (Prelude.drop (n-1) xs)

    descifrar :: [String] -> [Int] -> [Bool]
    descifrar texto clave = fase1_5
        where
            fase1   = partirMasResto (unirStrings texto) (Prelude.length clave)
            fase1_5 = columnasConEspacios (toLista fase1)
            fase2   = transponerLista fase1 clave
            fase3   = addEspacios (eliminarEspacios fase2) fase1_5 a
            fase4   = eliminarEspacios (transponerLista (partirMasResto fase3  (Prelude.length clave)) clave)
            a = (Prelude.length fase2) `div` (Prelude.length clave)
