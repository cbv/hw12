
(* current story: broken; attacks opponent's slot 0 once and zombies it. Working on making the zombie do something. *)

val print = EPrint.eprint

structure Gwillen1 :> LAYER =
struct
  structure GS = GameState

  open Kompiler

  (* This one doesn't pay any attention to the
     game state. *)
  fun init _ = let
        val _ = print "HELLO WORLD\n"
(*
        val exp = Lambda("_", Apply(Apply(Apply(Card Card.Attack, Card Card.Zero), Apply(Card Card.Succ, Card Card.Zero)), Apply(Card Card.Copy, Card Card.Zero)))  
*)
(*
        val exp = Lambda("X", Lambda("Y", Apply(Var("X"), Apply(Var "Y", Card Card.Zero))))
*)

(*
fun fix s = let
  val minifix = \"x" ` $"f" -- (\"y" ` $"x" -- $"x" -- $"y")
  val Z = \"f" ` minifix -- minifix (* " make fontify happy *)
in
  Apply (Z, s)
end

fun run_and_return_self src =
  fix (\"self" ` \"unused" ` Card LTG.Put -- src -- $"self")

fun for g = fix (\ "f" ` \ "x" ` (Card Card.Put)
                   -- (g -- $"x") -- (\ "q" ` $"f" -- (Card Card.Succ -- $"x")))
*)
        (*val exp = for (Card Card.I)*)

infix 9 --
val op -- = Apply
val $ = Var
fun \ x exp = Lambda (x, exp)
infixr 1 `
fun a ` b = a b

        fun for g s = \"X" ` (Card Card.Put) -- (g -- $"X") -- ((\"f" ` \"_" ` $"f" -- ((Card Card.Succ) -- $"X")) -- ((Card Card.Get) -- s))
        val exp = for (Card Card.I) (Int 0)
        val _ = print ((Kompiler.src2str exp) ^ "\n")
        val x = Kompiler.compile_no_clear_rev exp 3
        val _ = print ((LTG.turns2str x) ^ "\n")
     in
        ()
     end

  val _ = init ()

  open Macros
  open LTG

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
