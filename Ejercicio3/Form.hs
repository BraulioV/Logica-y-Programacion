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
    asociar [] _ r        = r
    asociar (v:vars) bt r = asociar vars bt (r ++ map (\x -> (v, x)) bt)

    --tabla_verdad :: Expr -> [[Int]]
    --tabla_verdad e = map (\x -> x ++ [(interpret e aux)]) b
    --    where
    --        v   = vars e
    --        b   = booltable v
    --        aux = asociar v b 0 []

    tabla_verdad :: Expr ->  [[Int]]
    tabla_verdad e = tabla_verdadAux e b
        where
            v = vars e
            b = booltable v
            tabla_verdadAux :: Expr -> [[Int]] -> [[Int]]
            tabla_verdadAux e (b:bs) = map (\x -> [x,(interpret e aux)]) b
                where
                    aux = asociar v b []
 
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

