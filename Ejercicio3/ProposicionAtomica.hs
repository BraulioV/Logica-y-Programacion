module ProposicionAtomica (module ProposicionAtomica) where

    {--------------------------------------------------------------------------
        ------------------
            OPERADORES
        ------------------

            n ==>    Np      ¬p          not
            c ==>    Cpq     p -> q      ANpq    p->q = (¬p v q)
            k ==>    Kpq     p ^  q      &&
            o ==>    Opq     p v q       ||
            e ==>    Epq     p <-> q     (p->q)^(q->p)
    --------------------------------------------------------------------------}

    infix 9 `n`
    infix 3 `k`
    infix 2 `o`
    infix 2 `c`
    infix 2 `e`

    data Logic a = Logic
        {valor :: Bool
        }      | Logic a `C` Logic a

        deriving (Show)

    {-
        * Negacion *

        Considerando p = (Logic True), su uso es:

            *ProposicionAtomica> n p
            Logic {valor = False}
    -}
    
    n :: (Logic a) -> (Logic a)
    n a = Logic (not (valor a))

    {-
        * And lógico *

        Considerando p = (Logic True), q = (Logic False) y r = (Logic True), 
        su uso es:
        
            *ProposicionAtomica> p `k` q
            Logic {valor = False}
            *ProposicionAtomica> p `k` r
            Logic {valor = True}
    -}
    
    k :: (Logic a) -> (Logic a) -> (Logic a)
    p `k` q = Logic ((valor p) && (valor q))

    {-
        * Or lógico *

        Considerando p = (Logic True), q = (Logic False) y r = (Logic True), 
        su uso es:
        
            *ProposicionAtomica> p `k` q
            Logic {valor = True}
            *ProposicionAtomica> p `k` r
            Logic {valor = True}
            *ProposicionAtomica> q `k` q
            Logic {valor = False}
    -}
    
    o :: (Logic a) -> (Logic a) -> (Logic a)
    p `o` q = Logic ((valor p) || (valor q))

    {-
        * Implicación *

        Considerando p = (Logic True), q = (Logic False) y r = (Logic True), 
        su uso es:
        
            *ProposicionAtomica> p `c` q
            Logic {valor = False}
            *ProposicionAtomica> p `c` r
            Logic {valor = True}
            *ProposicionAtomica> q `c` r
            Logic {valor = True}
            *ProposicionAtomica> q `c` q
            Logic {valor = True}
    -}
    
    c :: (Logic a) -> (Logic a) -> (Logic a)
    p `c` q = (n p) `o` q
    
    {-
        * Doble Implicación *

        Considerando p = (Logic True), q = (Logic False) y r = (Logic True), 
        su uso es:
        
            *ProposicionAtomica> p `e` q
            Logic {valor = False}
            *ProposicionAtomica> p `e` r
            Logic {valor = True}
            *ProposicionAtomica> q `e` r
            Logic {valor = False}
            *ProposicionAtomica> q `e` q
            Logic {valor = True}
    -}
    
    e :: (Logic a) -> (Logic a) -> (Logic a)
    p `e` q = (p `c` q) `k` (q `c` p)

