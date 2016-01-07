module Conjunto (module Conjunto) where

    import Data.List

    class TiposConNormalizacion b where
        norm :: b -> b


    data Conjunto a = Conjunto [a] deriving Show

    emptySet :: Conjunto a
    emptySet = Conjunto []

    noRepeats :: Eq a => [a] -> [a]
    noRepeats [] = []
    noRepeats [x] = [x]
    noRepeats (x:xs) = x:(filter (/= x) (noRepeats xs))

    qsort_e :: (Ord a) => [a] -> [a]
    qsort_e [] = []
    qsort_e (x:xs) = qsort_e (filter (< x) xs) ++ [x] ++ qsort_e(filter(>= x) xs)

    instance (Eq a, Ord a) => TiposConNormalizacion (Conjunto a) where
        norm (Conjunto a) = Conjunto (noRepeats(qsort_e a))

    instance (Eq a, Ord a) => Eq (Conjunto a) where
        (Conjunto a) == (Conjunto b) = [a] == [b]

    -- Union

    union_conj :: [a] -> [a] -> [a]
    union_conj [] []     = []
    union_conj [] xs     = xs
    union_conj xs []     = xs
    union_conj xs (y:ys) = union_conj (y:xs) ys

    infix 9 +:
    (+:) :: (Eq a, Ord a) => Conjunto a -> Conjunto a -> Conjunto a
    (Conjunto a) +: (Conjunto b) = norm (Conjunto (union_conj a b))

    -- Insercion de un elemento
    insert_aux :: [a] -> a -> [a]
    insert_aux xs x = x:xs

    insert :: (Eq a, Ord a) => Conjunto a -> a -> Conjunto a
    insert (Conjunto a) x = norm (Conjunto (insert_aux a x))

    -- Interseccion
    infix 9 /:
    (/:) :: (Eq a, Ord a) => Conjunto a -> Conjunto a -> Conjunto a
    (Conjunto a) /: (Conjunto b) = norm (Conjunto (a `intersect` b))

    -- Interseccion
    infix 9 -:
    (-:) :: (Eq a, Ord a) => Conjunto a -> Conjunto a -> Conjunto a
    (Conjunto a) -: (Conjunto b) = norm (Conjunto (c \\ d))
        where
            (Conjunto c) = norm (Conjunto a)
            (Conjunto d) = norm (Conjunto b)

    delete_elem :: (Eq a, Ord a) => Conjunto a -> a -> Conjunto a
    delete_elem (Conjunto a) n = norm (Conjunto (delete n b))
        where b = noRepeats a

    card :: (Eq a, Ord a) => Conjunto a -> Int
    card (Conjunto a) = length b
        where 
            (Conjunto b) = norm (Conjunto a)