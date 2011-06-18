
(* current story: broken; attacks opponent's slot 0 once and zombies it. Working on making the zombie do something. *)

val print = EPrint.eprint

structure Gwillen1 :> LAYER =
struct
  structure GS = GameState

  open Kompiler

  (* This one doesn't pay any attention to the
     game state. *)
  fun init _ = let
        (*val _ = print "HELLO WORLD\n"
        val exp = Lambda("_", Apply(Apply(Apply(Card Card.Attack, Card Card.Zero), Apply(Card Card.Succ, Card Card.Zero)), Apply(Card Card.Copy, Card Card.Zero)))  *)
        val exp = Lambda("X", Apply(Var("A"), Apply(Var "X", Var "B")))
        val _ = print ((Kompiler.src2str exp) ^ "\n")
        val x = Kompiler.compile exp 3
        val _ = print ((LTG.turns2str x) ^ "\n")
     in
        ()
     end

  val _ = init ()

  fun curry2 f x y = f(x, y)
  fun rep n x = List.concat (List.tabulate (n, fn _ => x))

  open LTG

  val L = curry2 LTG.LeftApply
  val R = curry2 LTG.RightApply

  val Z = Zero
  val Su = Succ

  (* cost *)
  (*      2 *) fun rap n = [L K n, L S n] (* reverse apply *)
  (* 3m + 4 *) fun apply_slot_to_slot n m = (rap n) @ [R n Get] @ (rep m ((rap n) @ [R n Succ])) @ [R n Z]
  (* 3m + 1 *) fun apply_slot_to_int n 0 = [R n Z]
                 | apply_slot_to_int n m = (rap n) @ [R n Succ] @ (rep (m-1) ((rap n) @ [R n Succ])) @ [R n Z]
  (*      1 *) fun dbl n = [L Dbl n]
  (*      1 *) fun succ n = [L Su n]
  (*      1 *) fun fastload n card = [R n card] (* n must hold ID! *)
  (*      1 *) fun fastzero n = fastload n Z (* n must hold ID! *)
  (*      1 *) fun apply a b = [R a b]

  (* must start with slot containing zero! *)
  (* cost is number of bits in n, plus number of 1 bits in n, minus one*)
  fun buildnum slot 0 = []
    | buildnum slot n = if (n mod 2 = 1) then (buildnum slot (n - 1)) @ (succ slot)
                                         else (buildnum slot (n div 2)) @ (dbl slot)

  (* cost is buildnum slot n,  + 1 *)
  fun fastnum slot n = (fastzero slot) @ (buildnum slot n)

  fun strat enemy mine1 mine2 = ref (

                 (* build our zombie function in slot 3 *)
                 (* zombies are applied to ID! *)
                 (* We want: Attack 0 1 8192 which will heal 0 and 1 *)
		 (* We will implement this as: Attack 0 1 (Copy 0) *)
                 (*fastload 3 *)

                 fastnum 2 enemy @ (* load enemy slot number into our slot 2 *)
                 fastnum 0 8192 @ (* slot 0 holds 8192 (amt. of damage) *)

                 fastload 1 Attack @
		 apply_slot_to_int 1 mine1 @
		 apply_slot_to_slot 1 2 @
		 apply_slot_to_slot 1 0 @ (* executes Attack mine1 enemy 8192 *)

		 fastload 1 Attack @
		 apply_slot_to_int 1 mine2 @
		 apply_slot_to_slot 1 2 @
		 apply_slot_to_slot 1 0 @ (* executes Attack mine2 enemy 8192 *)

		 fastload 1 Zombie @
		 apply_slot_to_slot 1 2 @
		 apply_slot_to_int 1 0 (* executes Zombie enemy 0 *)
  )

  val strategy = strat 255 1 0

  fun taketurn _ = case (!strategy) of nil => L Z 0
    | today::future => let 
      val _ = strategy := future
    in
      today
    end

end

structure Player = LayerFn(Gwillen1)
