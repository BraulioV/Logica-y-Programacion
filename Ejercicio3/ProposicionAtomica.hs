module ProposicionAtomica (module ProposicionAtomica) where

    class TiposConNormalizacion b where
        norm :: b -> b

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
    --infix 2 `e`

    data Logic a = Logic {valor :: Int} deriving(Show, Eq, Ord)

    instance Integral a => TiposConNormalizacion (Logic a) where
        norm (Logic a)
            | a /= 0    = Logic 1
            | otherwise = Logic 0

    {-
        * Negacion *

        Considerando p = (Logic True), su uso es:

            *ProposicionAtomica> n p
            Logic {valor = False}
    -}
    
    n :: Integral a => (Logic a) -> (Logic a)
    n a = Logic ((val + 1)`mod`2)
        where
            val = valor aux 
            aux = (norm (a))

    {-
        * And lógico *

        Considerando p = (Logic True), q = (Logic False) y r = (Logic True), 
        su uso es:
        
            *ProposicionAtomica> p `k` q
            Logic {valor = False}
            *ProposicionAtomica> p `k` r
            Logic {valor = True}
    -}
    
    k :: Integral a => (Logic a) -> (Logic a) -> (Logic a)
    p `k` q = Logic ((valor (norm p)) * (valor (norm q)))

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
    
    o :: Integral a => (Logic a) -> (Logic a) -> (Logic a)
    p `o` q = norm (Logic (alpha*beta + alpha + beta))
        where
            alpha = valor (norm p)
            beta = valor (norm q)


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
    
    c :: Integral a => (Logic a) -> (Logic a) -> (Logic a)
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
    
    e :: Integral a => (Logic a) -> (Logic a) -> (Logic a)
    p `e` q = (p `c` q) `k` (q `c` p)