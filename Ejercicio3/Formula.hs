module Formula (module Formula) where

    import Arbol

    data Formula = ArbolB Char

    quitarParentesis :: String -> String
    quitarParentesis xs = quitarParentesis' (filter (/= ')') xs)
        where
            quitarParentesis' :: String -> String
            quitarParentesis' xs = filter (/='(') xs

    crear :: String -> ArbolB Char
    crear xs
        | (head xs) == '(' = crearCaso2 ps ((length ps) - 1)
        | otherwise = crearCaso1 ps
            where
                ps = quitarParentesis xs
    
    crearCaso2 :: String -> Int -> ArbolB Char
    crearCaso2 xs 0 = hojaB (head xs)
    crearCaso2 xs n = NodoB (crearCaso2 xs (n-2)) (xs !! (n-1)) (hojaB (xs !! n))

    crearCaso1 :: String -> ArbolB Char
    crearCaso1 []  = VacioB
    crearCaso1 [x] = hojaB x
    crearCaso1 xs  = NodoB (hojaB (xs !! 0)) (xs !! 1) (crearCaso1 (drop 2 xs))
