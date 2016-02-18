module Formula (module Formula) where

    import Arbol

    data Formula = ArbolB Char

    quitarParentesis :: String -> String
    quitarParentesis xs = quitarParentesis' (filter (/= ')') xs)
        where
            quitarParentesis' :: String -> String
            quitarParentesis' xs = filter (/='(') xs

    eliminarEspacios :: String -> String
    eliminarEspacios xs = filter (/= ' ') xs

    {-
        Caso 1: a v (b -> c)
        Caso 1.1: ¬a v (b -> c)
        Caso 1.2: a v (b -> ¬c)
        Caso 2: (a v b) -> c
        Caso 2.1: (a v b) -> ¬c
        Caso 2.2: (¬a v b) -> c
    -}
    crear :: String -> ArbolB Char
    crear xs
        | (head xs) == '(' = crearCaso2 ps ((length ps) - 1)
        | otherwise = crearCaso1 ps
            where
                ps = eliminarEspacios (quitarParentesis xs)
    
    crearCaso2 :: String -> Int -> ArbolB Char
    crearCaso2 xs 0 = hojaB (head xs)
    crearCaso2 xs 1 = NodoB (hojaB (xs !! 1)) (head xs) VacioB
    crearCaso2 xs n 
        | (xs !! (n-1)) == 'n' = NodoB (crearCaso2 xs (n-3)) (xs !! (n-2)) (NodoB (hojaB (xs !! n)) (xs !! (n-1)) VacioB)
        | otherwise            = NodoB (crearCaso2 xs (n-2)) (xs !! (n-1)) (hojaB (xs !! n))

    crearCaso1 :: String -> ArbolB Char
    crearCaso1 []    = VacioB
    crearCaso1 [x]   = hojaB x
    crearCaso1 [x,y] = NodoB (hojaB y) x VacioB
    crearCaso1 xs  
        | (xs !! 0) == 'n' = NodoB (NodoB (hojaB (xs !! 1)) (xs !! 0) VacioB) (xs !! 2) (crearCaso1 (drop 3 xs))
        | otherwise        = NodoB (hojaB (xs !! 0)) (xs !! 1) (crearCaso1 (drop 2 xs))
