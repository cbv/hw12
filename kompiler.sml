structure Kompiler (* : KOMPILER *) =
struct

datatype src =
         Var of string
       | Lambda of string * src
       | Apply of src * src
       | Card of Card.card
       | Int of int

fun src2str (Var x) = x
  | src2str (Int i) = Int.toString i
  | src2str (Lambda (var, src)) = "(\\" ^ var ^ (src2str src) ^ ")"
  | src2str (Apply (x, y)) = "(" ^ (src2str x) ^ " " ^ (src2str y) ^ ")"
  | src2str (Card c) = Card.card2str c

exception Kompiler of string

(* Y' = S S K (S (K (S S (S (S S K)))) K) *)
(* XXX spoons this should be faster, but seems buggy
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

fun seq a b = (\"_" ` b) -- a

(* seqlist : src list -> src
*  seqlist [x1...xn] implements (x1; ... ; xn) *)
fun seqlist [] = Card (Card.I)
  | seqlist [a] = a
  | seqlist (a::ays) = (\"_" ` (seqlist ays)) -- a 

fun fix s = let
  val minifix = \"x" ` $"f" -- (\"y" ` $"x" -- $"x" -- $"y")
  val Z = \"f" ` minifix -- minifix (* " make fontify happy *)
in
  Apply (Z, s)
end

fun run_and_return_self src =
  fix (\"self" ` \"unused" ` Card LTG.Put -- src -- $"self")

fun rrs_ref src s =
  \"unused" ` ((\ "self" ` Card LTG.Put -- src -- $"self") -- ((Card Card.Get) -- (Int s)))

fun for g = fix (\ "f" ` \ "x" ` (Card Card.Put) 
                   -- (g -- $"x") -- (\ "q" ` $"f" -- (Card Card.Succ -- $"x")))

(* s is a slot number; this will only work in slot s *)
fun for_ref g s = \ "X" ` (Card Card.Put) -- (g -- $"X") -- ((\ "f" ` \ "q" ` $"f" -- ((Card Card.Succ) -- $"q")) -- ((Card Card.Get) -- (Int s)))

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
        | A (x, s as KApply (s1, s2)) =
          let
            fun contains x (KVar y) = x = y
              | contains x (KCard _) = false
              | contains x (KApply (s1, s2)) =
                  contains x s1 orelse contains x s2

            (* purity of combinator terms.

               these terms have side effects:

                get i       (read effect only)
                inc i
                dec i
                attack i j n
                help i j n
                copy i      (read effect only)
                revive i
                zombie i x

               any term containing one of these terms as a subterm
               has side effects.

              (XXX What about the side effect of causing errors? 
               maybe we want to just have the behavior that the
               optimizer is allowed to optimize away errors. -tom7)

              all other terms are pure. *)
            (* XXX OBSOLETE -- the current optimizer does not use this -wjl *)
            fun pure (KApply (KCard Card.Get, _)) = false
              | pure (KApply (KCard Card.Inc, _)) = false
              | pure (KApply (KCard Card.Dec, _)) = false
              | pure (KApply (KApply (KApply (KCard Card.Attack, _), _), _)) = false
              | pure (KApply (KApply (KApply (KCard Card.Help, _), _), _)) = false
              | pure (KApply (KCard Card.Copy, _)) = false
              | pure (KApply (KCard Card.Revive, _)) = false
              | pure (KApply (KApply (KCard Card.Zombie, _), _)) = false
              | pure (KApply (s1, s2)) = pure s1 andalso pure s2
              | pure _ = true

            (* PERF: might instead make the translation return whether or not
               the result is a value, to avoid exponential double traversal.. *)
            fun value (KCard _) = true
              | value (KVar _) = true
              | value (KApply (KCard Card.Succ, v)) = value v
              | value (KApply (KCard Card.Dbl, v)) = value v
              | value (KApply (KCard Card.S, v)) = value v
              | value (KApply (KApply (KCard Card.S, v1), v2)) = value v1
                                                         andalso value v2
              | value (KApply (KCard Card.K, v)) = value v
              | value (KApply (KCard Card.Attack, v)) = value v
              | value (KApply (KApply (KCard Card.Attack, v1), v2)) = value v1
                                                              andalso value v2
              | value (KApply (KCard Card.Help, v)) = value v
              | value (KApply (KApply (KCard Card.Help, v1), v2)) = value v1
                                                            andalso value v2
              | value (KApply (KCard Card.Zombie, v)) = value v
              | value _ = false
          in
            (* William can vouch for these optimisations *)
            (* Note: to preserve CBV semantics, we must ensure that the result
               is always a value. *)
            if value s andalso not (contains x s) then
                (KApply (KCard Card.K, s))
            else if value s1 andalso not (contains x s1)
                             andalso s2 = KVar x then
                s1
            else
               (* naive translation: term is not safe for optimization *)
               KApply (KApply (KCard Card.S, A (x, s1)), A (x, s2))
          end
    in
      T s
    end

fun kil2str (KCard c) = Card.card2str c
  | kil2str (KApply (k1, k2))
    = "(" ^ kil2str k1 ^ " " ^ kil2str k2 ^ ")"
  | kil2str (KVar x) = x

val L = LTG.LeftApply
val R = LTG.RightApply

(* newer, different version below -wjl *)
fun kil2turns_spoons init k i = let
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
  rev (f (init, k))
end

(**** wjl version of kil2turns -- differently biased than spoons version ****)

(* in particular, it seems really good for right-nested things like numbers,
   but it performs *very* poorly on some other kinds of terms with a bit of
   left nesting, like timecube.  spoons's kil2turns is slightly poorer on
   numbers, but much better on timecube.  below, we implement kil2turns by
   running both and picking the smaller of the two. *)

(* a twig is a tree where no branch leaves the main trunk for more than a
   single step.  this is the kind of thing that can be easily put into a
   slot by left and right applications. *)
datatype twig =
    TCard of Card.card
  | TLApp of Card.card * twig
  | TRApp of twig * Card.card

infix <@ @>
fun TC c = TCard c
fun c <@ ts = TLApp (c, ts)
fun ts @> c = TRApp (ts, c)

fun twigapp (TCard c, ts) = c <@ ts
  | twigapp (ts, TCard c) = ts @> c
  (*
  | twigapp (TLApp (c, us), ts) = ???
        (c us) ts
     == (c us) (K ts us)
     == S c (K ts) us   // XXX not in twig form yet :(
  | twigapp (TRApp (us, c), ts) =
  *)
  | twigapp (ts, TLApp (c, us)) = twigapp (Card.S <@ (Card.K <@ ts) @> c, us)
  | twigapp (ts, TRApp (us, c)) = twigapp (Card.S <@ (Card.K <@ ts), us) @> c
    (*
        ts @ (c us)
        K ts us (c us)
        S (K ts) c us
    *)

fun kil2twig (KCard c) = TCard c
  | kil2twig (KApply (t, u)) = twigapp (kil2twig t, kil2twig u)
  | kil2twig (KVar x) = raise (Kompiler ("unbound variable: " ^ x))

fun twig2turns twig i =
    let fun t2t (TCard c) = [R (i, c)]
          | t2t (TLApp (c, ts)) = L (c, i) :: t2t ts
          | t2t (TRApp (ts, c)) = R (i, c) :: t2t ts
    in
        rev (t2t twig)
    end

fun kil2turns_wjl init k i = init @ twig2turns (kil2twig k) i

(**** end wjl version of kil2turns ****)

(* XXX experimental: try both, take the smaller.  performance problem? *)
fun kil2turns init k i =
    let val tspoons = kil2turns_spoons init k i
        val twjl = kil2turns_wjl init k i
    in
        if length tspoons < length twjl
        then tspoons
        else twjl
    end

(* Tom's peephole optimizer. 
   XXX This keeps going until no more optimizations can be applied,
   but this could sometimes cause us to blow our time bounds. Probably
   should have a timer for the worst case scenarios. *)
local
open Card
in
  fun optimize il =
    let
        (* True if evaluating the argument will have no effects.
           This can be massively expanded! *)
        (* c.f. pure above -- but that turned out not to be useful for my
           purposes -wjl *)
        fun effectless (KCard _) = true
          | effectless _ = false

        fun opt (KApply (KApply (KCard S, KCard K), KCard K)) = KCard I
          | opt (KApply (KCard I, exp)) = opt exp
          | opt (KApply (KCard K, KCard I)) = KCard Put
            (* ... *)
          | opt (KApply (i1, i2)) = KApply (opt i1, opt i2)
          | opt (KVar v) = raise Kompiler ("unbound var in optimizer? " ^ v)
          | opt (KCard c) = KCard c

        fun loop il =
            let val il' = opt il
            in
                if il = il'
                then il
                else
                    let in
                        (* eprint ("Optimized to\n" ^ kil2str il' ^ "\n"); *)
                        loop il'
                    end
            end
    in
        (* eprint ("OPTIMIZE:\n" ^ kil2str il ^ "\n"); *)
        loop il
    end
end

fun compile s i =
    kil2turns [L(Card.Put, i)]
              (optimize (src2kil s)) i
fun compile_no_clear s i =
    kil2turns [L(Card.K, i), L(Card.I, i), L(Card.S, i)]
              (optimize (src2kil s)) i

(* This doesn't work and makes no sense and don't use it. --gwillen *)
fun compile_no_clear_rev s i =
    kil2turns [] (optimize (src2kil s)) i

val print = EPrint.eprint

fun test () = (
    print (kil2str (src2kil (Lambda("x", Var "x"))));
    print "\n";
    print (kil2str (src2kil (Lambda("x", Card Card.K))));
    print "\n";
    print (kil2str (src2kil (Lambda("x", Lambda ("y", Var "x")))));
    print "\n";
    print (kil2str (src2kil (Lambda("x", Lambda("y", Lambda("z",
        Apply(Apply(Var "x", Var "z"), Apply(Var "y", Var "z"))
    ))))));
    print "\n";
    print (LTG.turns2str (compile (Lambda("x", Lambda ("y", Var "x"))) 13));
    print "\n";
    print (LTG.turns2str (compile (Lambda("x", Int 4)) 1));
    print "\n";
    print (LTG.turns2str (compile (Int 4) 1));
    print "\n";
    print (LTG.turns2str (compile (Card Card.Attack -- Card Card.Zero -- Card Card.Zero -- Card Card.Zero) 1));
    print "\n";
    ())

(* a small test case that distinguishes kil2turns_spoons and kil2turns_wjl *)
val e = Lambda ("x", Lambda ("y", Lambda ("z", $"x" -- (Card Card.Attack -- $"x" -- $"y" -- $"z"))));

fun tomtest () =
    let in
        eprint (kil2str (src2kil (Lambda("x", Lambda ("y", Var "y")))));
        eprint "\n"
    end

end

