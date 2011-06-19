
(* reimplementation of spoons's thing *)

(* rapp : twig * kil -> twig *)
fun rapp (TCard c, k) = c <@ kil2twig k
  | rapp (ts, KCard c) = ts @> c
  | rapp (ts, KApply (KCard c, t)) = rapp (Card.S <@ (Card.K <@ ts) @> c, t)
  | rapp (ts, KApply (KApply (t, u), v)) =
        rapp (rapp (rapp ((Card.S <@ (Card.K <@ (Card.S <@ (Card.K <@ ts)))),
                          t),
                    u),
              v)
  | rapp (ts, KApply (KVar x, _)) = raise (Kompiler ("unbound variable: " ^ x))
  | rapp (ts, KVar x) = raise (Kompiler ("unbound variable: " ^ x))

(* rappho : kil -> (twig -> twig) -> twig *)
fun rappho (KCard c) r = r (TCard c)
  | rappho (KApply (KCard c, t)) r = rappho t (fn tt => r (c <@ tt))
  | rappho (KApply (t, KCard c)) r = rappho t (fn tt => r (tt @> c))
  | rappho (KApply (t, KApply (u, v))) r =
  | rappho (KApply (KApply (t, u), v)) r =
        rappho t (fn tt =>
        rappho u (fn uu =>
        rappho v (fn vv =>
            (* build (tt uu) vv, and feed it to r *)
            (*
                (tt uu) vv
             == (tt uu) (K vv uu)
             == S tt (K vv) uu
             == (K (S tt) vv) (K vv) uu
             == S (K (S tt)) K vv uu

            *)
        )))

  | rapp (KApply (KVar x, _)) _ = raise (Kompiler ("unbound variable: " ^ x))
  | rapp (KVar x) _ = raise (Kompiler ("unbound variable: " ^ x))

fun kil2twig k = rapp (TCard Card.I) k

fun kil2twigho k = rapp (fn tt => tt) k

(* lapp : kil * twig -> twig *)
(*
fun lapp (KCard c, ts) = TLApp (c, ts)
  | lapp (KApply (
*)

