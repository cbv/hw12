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
(* ((\_. b) a)* == (\_. b)* a*
                == ([x] b)* a* *)
(*
    dec 0; dec 1 == S (K Dec) (K 1) (Dec 0)
                --> S (K Dec) (K 1) I       // boom
                --> K Dec I (K 1 I)
                --> Dec (K 1 I)
                --> Dec 1
                --> I                       // boom

    dec 0; attack 1 0 0 == S (K (Attack 1 0)) (K 0) (Dec 0)
                       --> S (K (Attack 1 0)) (K 0) I       // boom
                       --> K (Attack 1 0) I (K 0 I)
                       --> K (Attack 1 0) I 0
                       --> Attack 1 0 0
                       --> I                                // boom

    idea: optimize: "S (K t) (K u) e" to "e (t u)", when e is an effectful
        term that returns I and t and u are both values

    S (K t) (K u) e -->  S (K t) (K u) I        // effect
                    -->  K t I (K u I)
                    --> t u
                    -->* whatever               // effects?

    e (t u) -->  I (t u)        // effect
            -->* I whatever     // effects?
            -->  whatever
*)

(* seqlist : src list -> src
*  seqlist [x1...xn] implements (x1; ... ; xn) *)
fun seqlist [] = Card (Card.I)
  | seqlist [a] = a
  | seqlist (a::ays) = (\"_" ` (seqlist ays)) -- a 

(* XXX this has never been tested! spoons should check it out. *)
fun elet (var : string, value : src, exp : src) = Apply(Lambda(var, exp), value)

fun fix s = let
  val minifix = \"x" ` $"f" -- (\"y" ` $"x" -- $"x" -- $"y")
  val Z = \"f" ` minifix -- minifix (* " make fontify happy *)
in
  Apply (Z, s)
end

(* fun rrs src = fn unused => Put src (rrs src) *)
fun run_and_return_self src =
  fix (\"self" ` \"unused" ` Card LTG.Put -- src -- $"self")

(* fun rrs_ref src s = fn unused => (fn self => Put src self) (Get s) *)
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
        | T_int n = if n < 0 then raise (Kompiler "Int of negative number!") else
                    if n mod 2 = 0 then
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
              | contains x (KApply (s, t)) = contains x s orelse contains x t

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

(*
alternative version -- totally broken!
fun kil2turns_spoons init k i = let
  fun f (r, KCard c) = r ([R(i, c)])
    | f (r, KApply (KCard c, t)) = 
      f (fn ts_t => r (L(c, i) :: ts_t), t)
    | f (r, KApply (KApply (t, u), v)) = 
      f (fn ts_t => f (fn ts_u => f (fn ts_v =>
         r (L(Card.S, i) :: ts_t @ [L(Card.K, i)] @ ts_v @ ts_u),
         v), u), t)
    | f (r, KApply (KVar x, _))
      = raise (Kompiler ("unbound variable: " ^ x))
    | f (r, KVar x)
      = raise (Kompiler ("unbound variable: " ^ x))
  in
    rev (f (fn ts => ts @ init, k))
  end
*)

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

exception Kil2TwigTimeout

fun twigapp' r (TCard c, ts) = c <@ ts
  | twigapp' r (ts, TCard c) = ts @> c
  (*
  | twigapp (TLApp (c, us), ts) = ???
        (c us) ts
     == (c us) (K ts us)
     == S c (K ts) us   // XXX not in twig form yet :(
  | twigapp (TRApp (us, c), ts) =
  *)
  | twigapp' r (ts, TLApp(c,us)) = twigapp r (Card.S <@ (Card.K <@ ts) @> c, us)
  | twigapp' r (ts, TRApp(us,c)) = twigapp r (Card.S <@ (Card.K <@ ts), us) @> c
    (*
        ts @ (c us)
        K ts us (c us)
        S (K ts) c us
    *)

and twigapp r ts =
    let in
        r := !r - 1;
        if !r < 0
        then raise Kil2TwigTimeout
        else twigapp' r ts
    end

fun kil2twig' r (KCard c) = TCard c
  | kil2twig' r (KApply (t, u)) = twigapp r (kil2twig r t, kil2twig r u)
  | kil2twig' r (KVar x) = raise (Kompiler ("unbound variable: " ^ x))

and kil2twig r k =
    let in
        r := !r - 1;
        if !r < 0
        then raise Kil2TwigTimeout
        else kil2twig' r k
    end

fun kil2twig_limited k =
    let fun kilsize (KCard _) = 1
          | kilsize (KApply (t, u)) = kilsize t + kilsize u + 1
          | kilsize (KVar x) = 1
        val size = kilsize k
        val r = ref (100 * size)    (* overflow? *)
    in
        kil2twig r k
    end
    handle Overflow => raise Kil2TwigTimeout

fun twig2turns twig i =
    let fun t2t (TCard c) = [R (i, c)]
          | t2t (TLApp (c, ts)) = L (c, i) :: t2t ts
          | t2t (TRApp (ts, c)) = R (i, c) :: t2t ts
    in
        rev (t2t twig)
    end

fun kil2turns_wjl init k i = SOME (init @ twig2turns (kil2twig_limited k) i)
                             handle Kil2TwigTimeout => NONE

(**** end wjl version of kil2turns ****)

(* XXX experimental: try both, take the smaller.  performance problem? *)
(* NB: kil2turns_wjl will return NONE if it takes more than 100*n steps
   to convert a kil of size n *)
fun kil2turns init k i =
    let (* val _ = eprint "TRYING SPOONS\n" *)
        val tspoons = kil2turns_spoons init k i
        (* val _ = eprint "TRYING WOMBAT\n" *)
        val twjl_o = kil2turns_wjl init k i
        (* val _ = eprint "MOVING RIGHT ALONG\n" *)
    in
        case twjl_o of
            NONE => tspoons
          | SOME twjl =>
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

        fun opt (KApply (KApply (KCard S, KCard K), KCard K)) = KCard I
          | opt (KApply (KCard I, exp)) = opt exp
          | opt (KApply (KCard K, KCard I)) = KCard Put
         (*
          based on wjl's comment
          idea: optimize: "S (K t) (K u) e" to "e (t u)", when e is an effectful
                           term that returns I and t and u are both values

          | opt (KApply (KApply (KApply (KCard Card.S, 
                                         KApply (KCard Card.K, t)),
                                 KApply (KCard Card.K, u)),
                         e)) =
            let in
              (* XXX need to check for that e evals to I *)
              if pure t andalso pure u then
                eprint "[Kom] Got magic optimzation!"
              else ();
              KApply (opt e, KApply (opt t, opt u))
            end
          *)

          | opt (KApply (KApply (KCard K, t), u)) =
                if pure u
                then opt t
                else KApply (KApply (KCard K, opt t), opt u)
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

fun compile s i = let
      (* val _ = eprint "KILING\n" *)
      val kil = src2kil s 
      (* val _ = eprint "OPTING\n" *)
      val opt = optimize kil
      (* val _ = eprint "TRUNING\n" *)
      val turns = kil2turns [L(Card.Put, i)] opt i
      (* val _ = eprint "RETURNING\n" *)
   in
      turns
   end
(*
    kil2turns [L(Card.Put, i)]
              (optimize (src2kil s)) i
*)

fun compile_never_exponential s i = compile s i
    (*
    (* compile is now never exponential, by fiat *)
    kil2turns_spoons [L(Card.Put, i)]
                     (optimize (src2kil s)) i
    *)

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
    let 
        val getsrc = Card LTG.Get -- Int 5
        val gettarg = Card LTG.Get -- Int 8
        val amount = 8192

        (* XXX this is totally not done *)
        (* 365 *)
        val helpy_indirect1 =
            (\"amount" `
             (Card LTG.Revive -- gettarg) --
             (* First heal from source to target.
                This drains a lot of src's health, but gives even
                more to target. Doing this in reverse then restores
                health to src, but leaves target with any excess. *)
             (Card LTG.Help -- getsrc -- gettarg -- $"amount") --
             (Card LTG.Help -- gettarg -- getsrc -- $"amount")) --
            Int amount

        (* also 365 *)
        val helpy_indirect2 =
             (Card LTG.Revive -- gettarg) --
             ((\"amount" `
               (Card LTG.Help -- getsrc -- gettarg -- $"amount") --
               (Card LTG.Help -- gettarg -- getsrc -- $"amount")) -- Int amount)

        val getamount = Card LTG.Get -- Int 8

        val helpy_indirect3 =
            (Card LTG.Revive -- gettarg) --
            (Card LTG.Help -- gettarg -- gettarg -- getamount) --
            (Card LTG.Help -- gettarg -- gettarg -- getamount)

        val target = 123
        val src = 0

        val helpy_indirect =
            (\"src" ` \"target" ` \"amount" `
             (Card LTG.Revive -- $"target") --
             (* First heal from source to target.
                This drains a lot of src's health, but gives even
                more to target. Doing this in reverse then restores
                even more health to src, but leaves target
                with any excess. *)
             (* XXX Should try to do this more times? *)
             (Card LTG.Help -- $"src" -- $"target" -- $"amount") --
             (Card LTG.Help -- $"target" -- $"src" -- $"amount")) --
            Int src -- Int target -- Int amount


        val prog = (* run_and_return_self *) helpy_indirect
        val insns = compile prog 231
    in
        eprint ("Length of program: " ^ Int.toString (length insns) ^ "\n")
    end

end

