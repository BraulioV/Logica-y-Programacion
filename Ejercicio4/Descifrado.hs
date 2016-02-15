module Descifrado (module Descifrado) where

    import Data.Matrix      -- install with cabal install Matrix
    import Data.Vector

    noRepeats :: (Eq a, Ord a) => [a] -> [a]
    noRepeats [] = []
    noRepeats [x] = [x]
    noRepeats (x:xs) = x:(filter (/= x) (noRepeats xs))

    qsort_e :: (Eq a, Ord a) => [a] -> [a]
    qsort_e [] = []
    qsort_e (x:xs) = qsort_e (filter (< x) xs) ++ [x] ++ qsort_e(filter(>= x) xs)


    sortSr :: (Eq a, Ord a) => [a] -> [a]
    sortSr xs = noRepeats (qsort_e xs)

   partirMasResto :: String -> Int -> Matrix Char
    partirMasResto texto n
        | (Prelude.length texto) < n = error "longitud del mensaje menor a la clave"
        | b == 0             = Data.Matrix.fromList a n texto
        | otherwise          = Data.Matrix.fromList (a+1) n t
            where
                a = (Prelude.length texto) `div` n
                b = (Prelude.length texto) `mod` n
                t = addWhiteSpaces texto (n - b)