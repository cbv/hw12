
(* current story: broken; attacks opponent's slot 0 once and zombies it. Working on making the zombie do something. *)

val print = EPrint.eprint

structure Gwillen1 :> LAYER =
struct
  structure GS = GameState

  open Kompiler
  open Macros

open LTG

infix 9 --
val op -- = Apply
val $ = Var
fun \ x exp = Lambda (x, exp)
infixr 1 `
fun a ` b = a b

  (* This one doesn't pay any attention to the
     game state. *)
  fun init _ = let
        val _ = print "HELLO WORLD\n"
        val exp = [] (*zombie_help_0_0_copy_0 1*)
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
*)

(*
fun for_g = ` \ "x" ` (Card Card.Put) -- (g -- $"x") -- (\ "_" ` for_g -- (Card Card.Succ -- $"x")))
*)
        (*val exp = for (Card Card.I)*)


(*
        fun for g s = \"X" ` (Card Card.Put) -- (g -- $"X") -- ((\"f" ` \"_" ` $"f" -- ((Card Card.Succ) -- $"X")) -- ((Card Card.Get) -- s))
        val exp = for (Card Card.I) (Int 9)
*)
(*
        val _ = print ((Kompiler.src2str exp) ^ "\n")
        val x = Kompiler.compile_no_clear_rev exp 3
*)
        val _ = print ((LTG.turns2str exp) ^ "\n")
     in
        ()
     end

  val _ = init ()


  (* Zombie performs \_.Help (2*(Copy tgt)) (2(Copy tgt)+1) (Copy dmg) *)
  fun zombie tgt dmg slot = let
      val exp = \"_" ` Card Help -- (Card Dbl -- (Card Copy -- Int tgt)) -- (Card Succ -- (Card Dbl -- (Card Copy -- Int tgt))) -- (Card Copy -- Int dmg) 
    in
      (* this should be named like, compile_assume_identity *)
      Kompiler.compile_no_clear_rev exp slot
    end

  fun zombie_loader zombie s = Kompiler.compile_no_clear_rev (rrs_ref (
    Card Zombie -- Card Z -- (Card Get -- Int zombie)
(*
      fastload s Zombie @
      apply s Z @
      apply_slot_to_slot s zombie) s
*)
  ) s) s

  val is = Int.toString

  val strategy = let
      val dmg = 0
      val tgt = 1
      val zmb = 2
      val loader = 3   
      val result = ref (
        (fastest_doubleshot()) @
        clear 0 @
        fastnum dmg 10000 @
        fastnum tgt 0 @
        zombie tgt dmg zmb @
        zombie_loader zmb loader @
        (rep 128 (apply loader Z @ succ tgt)))
     
      val _ = print ("10k: " ^ (is (length (fastnum dmg 10000))) ^
                     "zombie: " ^ (is (length (zombie tgt dmg zmb))) ^
                     "loader: " ^ (is (length (zombie_loader zmb loader))) ^ "\n")
    in
      result
    end


  fun taketurn _ = case (!strategy) of nil => L Z 0
    | today::future => let 
      val _ = strategy := future
    in
      today
    end

end

structure Player = LayerFn(Gwillen1)
