structure Kompiler : KOMPILER =
struct

  val print = eprint

  datatype src =
      Var of string
    | Lambda of string * src
    | Apply of src * src
    | Card of Card.card

  exception Kompiler of string

  (* kombinator internal language *)
  datatype kil = KApply of kil * kil
               | KCard of Card.card
               | KVar of string

  (* first translate from lambda calculus to kombinators *)
  fun src2kil s =
    let
      fun T (Lambda (x, s)) = A (x, T s)
        | T (Apply (s1, s2)) = KApply (T s1, T s2)
        | T (Card c) = KCard c
        | T (Var x) = KVar x

     and A (x, KVar y) = if x = y then KCard Card.I
                         else KApply (KCard Card.K, KVar y)
       | A (x, KCard c) = KApply (KCard Card.K, KCard c)
       | A (x, KApply (s1, s2)) =
         KApply (KApply (KCard Card.S, A (x, s1)), A (x, s2))
   in
     T s
   end

  fun kil2str (KCard c) = Card.card2str c
    | kil2str (KApply (KCard c1, KCard c2))
      = Card.card2str c1 ^ " " ^ Card.card2str c2
    | kil2str (KApply (k1, k2))
       = "(" ^ kil2str k1 ^ ") (" ^ kil2str k2 ^ ")"
    | kil2str (KVar x) = x

  fun kil2turns k i =
    let
      val L = LTG.LeftApply
      val R = LTG.RightApply
  
      fun f (acc, KCard c) = R(i, c) :: acc
        | f (acc, KApply (KCard c, t))
          (* S(K acc) c t *)
          = f(R(i, c) :: L(Card.S, i) :: L(Card.K, i) :: acc, t)
        | f (acc, KApply (KApply (t, u), v))
          (* S(K(S(K acc))) t u v *)
          = f (f (f (L(Card.S, i) :: L(Card.K, i) 
                     :: L(Card.S, i) :: L(Card.K, i) :: acc, t), u), v)
        | f (acc, KApply (KVar x, _))
          = raise (Kompiler ("unbound variable: " ^ x))
        | f (acc, KVar x)
          = raise (Kompiler ("unbound variable: " ^ x))
    in
      rev (f ([L(Card.Put, i)], k))
    end

  fun compile s i = kil2turns (src2kil s) i

fun test () = (
    print (kil2str (src2kil (Lambda("x", Var "x"))));
    print "\n";
    print (kil2str (src2kil (Lambda("x", Card Card.K))));
    print "\n";
    print (kil2str (src2kil (Lambda("x", Lambda ("y", Var "x")))));
    print "\n";
    print (LTG.turns2str (compile (Lambda("x", Lambda ("y", Var "x"))) 13));
    print "\n";
    ()
  )

end

