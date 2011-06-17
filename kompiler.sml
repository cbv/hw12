structure Kompiler : KOMPILER =
struct

datatype src =
         Var of string
       | Lambda of string * src
       | Apply of src * src
       | Card of Card.card
       | Int of int

exception Kompiler of string

(* Y' = S S K (S (K (S S (S (S S K)))) K) *)
(*
fun fix s = let
  val S = Card Card.S
  val K = Card Card.K
  val A = Apply
in
  A (A (A (A (S, S), K),
        A (A (S, A (K, A (A (S, S), 
                          A (S, A (A (S, S), K))))), K)),
     s)
end
*)

  infix 9 --
  val op -- = Apply
  val $ = Var
  fun \ x exp = Lambda (x, exp)
  infixr 1 `
  fun a ` b = a b

  fun fix s = let
          val minifix =
              \"x" ` $"f" -- (\"y" ` $"x" -- $"x" -- $"y")
          val Z = \"f" ` minifix -- minifix
      in
          Apply (Z, s)
      end


(* kombinator internal language *)
datatype kil = KApply of kil * kil
             | KCard of Card.card
	     | KVar of string

(* first translate from lambda calculus to kombinators *)
fun src2kil s =
    let
      (* helper function to encode numbers *)
      fun T_int 0 = KCard Card.Zero
        | T_int n = if n mod 2 = 0 then
	               KApply (KCard Card.Dbl, T_int (n div 2))
	             else KApply (KCard Card.Succ, T_int (n - 1))

      fun T (Lambda (x, s)) = A (x, T s)
        | T (Apply (s1, s2)) = KApply (T s1, T s2)
        | T (Card c) = KCard c
        | T (Var x) = KVar x
        | T (Int n) = T_int n

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

fun kil2turns k i = let
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

val print = EPrint.eprint

fun test () = (
    print (kil2str (src2kil (Lambda("x", Var "x"))));
    print "\n";
    print (kil2str (src2kil (Lambda("x", Card Card.K))));
    print "\n";
    print (kil2str (src2kil (Lambda("x", Lambda ("y", Var "x")))));
    print "\n";
    print (LTG.turns2str (compile (Lambda("x", Lambda ("y", Var "x"))) 13));
    print "\n";
    print (LTG.turns2str (compile (Lambda("x", Int 4)) 1));
    print "\n";
    ())

end

