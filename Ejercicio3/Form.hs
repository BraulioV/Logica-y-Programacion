module Form (module Form) where

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

    leer :: String -> Expr
    leer (x:xs)
        | x == '!'  = N (leer xs)
        | x == '&'  = K (leer xs)
        | x == '>'  = C (leer xs)
        | x == '<'  = E (leer xs)
        | x == '|'  = O (leer xs)
        | otherwise = leer xs