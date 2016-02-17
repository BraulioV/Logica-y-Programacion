module LogicaProposicional (module LogicaProposicional) where

    class TiposConNormalizacion b where
        norm :: b -> b
    {-----------------------------------------------------------
        ------------------
            OPERADORES
        ------------------

            Np      ¬p          not
            Cpq     p -> q      ANpq    p->q = (¬p v q)
            Kpq     p ^  q      &&
            Apq     p v q       ||
            Epq     p <-> q     (p->q)^(q->p)
    ------------------------------------------------------------}

    data Logic a = Logic
        {
            valor :: Bool
        } deriving (Show)

    infixl 9 `n`
    n :: (Logic a) -> (Logic a)
    n a = Logic (not (valor a))