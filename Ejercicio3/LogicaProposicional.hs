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

    infix 7 ¬
    infix 7 :>
    infix :^
    infix :|
    infix :<->:

    