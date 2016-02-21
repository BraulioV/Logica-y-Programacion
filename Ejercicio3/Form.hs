module Form (module Form) where

    import Data.List (nub)
    import Data.Maybe (fromMaybe)

    data Expr = Variable Char
                | N Expr        -- negacion
                | C Expr Expr   -- implicacion
                | K Expr Expr   -- and
                | O Expr Expr   -- or
                | E Expr Expr   -- doble implicacion
                deriving Eq

    instance Show Expr where
        show (Variable name) = show name
        show (N expr)        = '¬':show expr
        show (C expr1 expr2) = show expr1 ++ '-':'>':show expr2
        show (K expr1 expr2) = show expr1 ++ '^':show expr2
        show (O expr1 expr2) = show expr1 ++ 'v':show expr2
        show (E expr1 expr2) = show expr1 ++ '<':'-':'>':show expr2

    -- obtener todas las variables de una formula
    variables :: Expr -> [Char]
    variables (Variable name) = [name]
    variables (N expr)        = variables expr
    variables (C expr1 expr2) = variables expr1 ++ variables expr2
    variables (K expr1 expr2) = variables expr1 ++ variables expr2
    variables (O expr1 expr2) = variables expr1 ++ variables expr2
    variables (E expr1 expr2) = variables expr1 ++ variables expr2

    -- sin repetir  
    vars :: Expr -> [Char]
    vars = nub . variables

    -- obtener toda la combinacion de 0 y 1 para las variables
    booltable :: [Char] -> [[Int]]
    booltable []     = [[]]
    booltable (a:as) = [b:r | b <- [0,1], r <- booltable as]

    asociar :: [Char] -> [Int] -> [(Char, Int)] -> [(Char, Int)]
    asociar [] _ r            = r
    asociar (v:vars) (b:bt) r = asociar vars bt ((v,b):r)

    tabla_verdad :: Expr ->  [Int]
    tabla_verdad e = reverse (tabla_verdadAux e b [])
        where
            b = booltable (vars e)
            tabla_verdadAux :: Expr -> [[Int]] -> [Int] -> [Int]
            tabla_verdadAux _ [] r     = r
            tabla_verdadAux e (b:bs) r = tabla_verdadAux e bs ((t_v e b) ++ r)
                where
                    t_v :: Expr -> [Int] -> [Int]
                    t_v e b = [interpret e aux]
                        where 
                            aux = asociar (vars e) b []
 
    interpret :: Expr -> [(Char, Int)] -> Int
    interpret (Variable v)  vs = fromMaybe 0 (lookup v vs)
    interpret (N      expr) vs = ((interpret expr vs)+1)`mod`2
    interpret (K exp1 exp2) vs = (interpret exp1 vs) * (interpret exp2 vs)
    interpret (O exp1 exp2) vs = ((interpret exp1 vs) * (interpret exp2 vs) 
                                        + (interpret exp1 vs) + (interpret exp2 vs)) `mod` 2
    interpret (C exp1 exp2) vs = interpret (O (N exp1) (exp2)) vs
    interpret (E exp1 exp2) vs 
        | (interpret exp1 vs) == (interpret exp2 vs) = 1
        | otherwise                                  = 0

    formaNormalNegada :: Expr -> Expr
    formaNormalNegada atomo@(Variable _)        = atomo
    formaNormalNegada atomo@(N (Variable _))    = atomo
    formaNormalNegada (N (N atomo))             = atomo
    formaNormalNegada (K exp1 exp2)             = (K (formaNormalNegada exp1) (formaNormalNegada exp2))
    formaNormalNegada (N (K exp1 exp2))         = formaNormalNegada (O (N exp1) (N exp2))
    formaNormalNegada (O exp1 exp2)             = (O (formaNormalNegada exp1) (formaNormalNegada exp2))
    formaNormalNegada (N (O exp1 exp2))         = formaNormalNegada (K (N exp1) (N exp2))
    formaNormalNegada (C exp1 exp2)             = formaNormalNegada (O (N exp1) exp2)
    formaNormalNegada (N (C exp1 exp2))         = formaNormalNegada (K exp1 (N exp2))
    formaNormalNegada (E exp1 exp2)             = formaNormalNegada (O (K exp1 exp2) (K (N exp1) (N exp2)))
    formaNormalNegada (N (E exp1 exp2))         = formaNormalNegada (K (O exp1 exp2) (O (N exp1) (N exp2)))

    formaNormalDisyuntiva ::  Expr -> Expr
    formaNormalDisyuntiva = fndAux . formaNormalNegada
        where
            fndAux :: Expr -> Expr
            fndAux (K exp1 exp2) = (O (fndAux exp1) (fndAux exp2))
            fndAux (O exp1 exp2) = (O (fndAux exp1) (fndAux exp2))
            fndAux atomo         = atomo

    -- saber si una formula es tautologia o no
    es_tautologia :: Expr -> Bool
    es_tautologia e = 
        case foldl (*) 1 (tabla_verdad e) of
            0         -> False
            otherwise -> True
