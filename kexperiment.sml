
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

(* rappho : (twig -> twig) * kil -> twig *)
fun rappho (r, KCard c) = r (TCard c)
  | rappho (r, KApply (KCard c, t)) =
        rappho (fn ts =>  , t)

fun kil2twig k = rapp (TCard Card.I) k

(* lapp : kil * twig -> twig *)
(*
fun lapp (KCard c, ts) = TLApp (c, ts)
  | lapp (KApply (
*)

