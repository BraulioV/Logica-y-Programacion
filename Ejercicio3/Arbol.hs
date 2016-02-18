module Arbol (module Arbol) where 

    data Arbol a = Vacio
                 | Nodo a [Arbol a]
            --   | Hoja a []
                 deriving Show

    -- Definición de una hoja de cualquier tpo

    hoja :: a -> Arbol a
    hoja x = Nodo x []

    -- Definimos el arbol de enteros
    a1 :: Arbol Integer
    a1 = Nodo 10 [a11, a12, a13]
        where
            a11 = Nodo 22[hoja 15, hoja 12]
            a12 = hoja 35
            a13 = Nodo 52 [hoja 33]

    {- El árbol anterior, se corresponde con el arbol que hay a continuación
                    10
               __/  |   \___
            22      35      52
           /  \              |
        15      12          33
    -}

    raiz :: Arbol a -> a
    raiz Vacio = error "Arbol vacio -> no tiene raiz"
    raiz (Nodo x _) = x

    -- Hacemos el tamaño para cada nodo, los metemos en una lista, y los sumamos

    size :: Arbol a -> Int
    size Vacio = 0
    size (Nodo _ xs) = (+1).sum.map size $ xs

    profundidad :: Arbol a -> Int
    profundidad Vacio = 0
    profundidad (Nodo _ []) = 1 -- hojas 
    profundidad (Nodo _ xs) = (+1).maximum.map profundidad $ xs

    nivel :: Int -> Arbol a -> [a]
    nivel i Vacio = error ("Arbol vacío no tiene el nivel " ++ show i)
    nivel 0 (Nodo x xs) = [x]
    nivel i (Nodo x []) = []
    nivel i (Nodo x xs) = concat [nivel (i-1) j | j <- xs]

    data ArbolB a = VacioB
                  | NodoB (ArbolB a) a (ArbolB a)
                  deriving Show

    hojaB :: a -> ArbolB a
    hojaB x = NodoB VacioB x VacioB

    a2 :: ArbolB Integer
    a2 = NodoB aI 10 aD
        where
            aI = NodoB aII 15 aID
            aD = NodoB aDI 18 aDD
            aII = hojaB 24
            aID = hojaB 27
            aDI = VacioB
            aDD = hojaB 24

    tamB :: ArbolB a -> Int
    tamB VacioB = 0
    tamB (NodoB i r d) = 1 + tamB i + tamB d

    profundidadB :: ArbolB a -> Int
    profundidadB VacioB = 0
    profundidadB (NodoB i _ d) = 1 + max (profundidadB i) (profundidadB d)

    raizB :: ArbolB a -> a
    raizB VacioB = error "Un arbol vacio no tiene raiz"
    raizB (NodoB _ x _) = x

    -- Recorrido del arbol binario

    enOrdenB :: ArbolB a -> [a]
    enOrdenB VacioB = []
    enOrdenB (NodoB i r d) = enOrdenB i ++ (r:enOrdenB d)

    preOrdenB :: ArbolB a -> [a]
    preOrdenB VacioB = []
    preOrdenB (NodoB i r d) = (r:preOrdenB i) ++ preOrdenB d

    postOrdenB :: ArbolB a -> [a]
    postOrdenB VacioB = []
    postOrdenB (NodoB i r d) = postOrdenB i ++ postOrdenB d ++ [r]

    -- Metemos el árbol en la clase Functor

    instance Functor ArbolB where
        fmap f VacioB = VacioB
        fmap f (NodoB i e d) = NodoB (fmap f i) (f e) (fmap f d)

    instance Functor Arbol where
        fmap f Vacio = Vacio
        fmap f (Nodo x xs) = Nodo (f x) (map (fmap f) xs)

    duplicar :: Functor f => f Integer -> f Integer
    duplicar = fmap (*2)

    -- plegado de árboles
    sumaArbolB :: ArbolB Integer -> Integer
    sumaArbolB VacioB = 0
    sumaArbolB (NodoB i r d) = sumar (sumaArbolB i) r (sumaArbolB d)
        where
            sumar :: Integer -> Integer -> Integer -> Integer
            sumar x y z = x + y + z

    enOrdenB' :: ArbolB a -> [a]
    enOrdenB' VacioB = []
    enOrdenB' (NodoB i r d) = concatenar (enOrdenB' i) r (enOrdenB' d)
        where
            concatenar x y z = x ++ (y:z)

    {-
        Con estas dos funciones se ve que el plegado se puede aplicar tanto a 
        los recorridos como a la suma, generalizando las funciones mediante el
        uso de la función foldr
    -}

    foldArbolB :: (b -> a -> b -> b) -> b -> ArbolB a -> b
    foldArbolB f z = fun
        where
            fun VacioB = z
            fun (NodoB i r d) = f (fun i) r (fun d)


    sumaArbolBf :: ArbolB Integer -> Integer
    sumaArbolBf = foldArbolB sumar 0
        where
            sumar x y z = x + y + z

    enOrdenBf :: ArbolB a -> [a]
    enOrdenBf = foldArbolB concatenar []
        where
            concatenar x y z = x ++ (y:z)

    {---------------------------------------------------------------------------
        Definir, usando la función de plegado, las funciones tamaño, 
        profundidadB
    ---------------------------------------------------------------------------}

    tamBf :: ArbolB Integer -> Integer
    tamBf = foldArbolB sumar 1
        where
            sumar x y z = x + z + y
