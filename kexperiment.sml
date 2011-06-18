
(* reimplementation of spoons's thing *)

(* rapp : twig * kil -> twig *)
fun rapp (ts, KCard c) = TRApp (ts, c)
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
  | rappho (KApply (KApply (t, u), v)) r =
        rappho t (fn tt =>
        rappho u (fn uu =>
        rappho v (fn vv =>
            (* build (tt uu) vv, and feed it to r *)
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

