module Form (module Form) where

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

    --leer :: String -> Expr
    --leer (x:xs)
    --    | x == '!'  = N (leer xs)
    --    | x == '&'  = K (leer xs)
    --    | x == '>'  = C (leer xs)
    --    | x == '<'  = E (leer xs)
    --    | x == '|'  = O (leer xs)
    --    | otherwise = leer xs

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