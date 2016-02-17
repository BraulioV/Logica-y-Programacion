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

    infix 9 ¬
    (¬) :: (Logic a) -> (Logic a)
    (¬) a = Logic (not (valor a))

    infix 7 ^
    (^) :: (Logic a) -> (Logic a) -> (Logic a)
    (^) a b = Logic ((valor a) && (valor b))

    --infix 7 :>

    --infix :|
    --infix :<->:
