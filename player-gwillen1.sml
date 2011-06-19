
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
  
  fun init _ = ()

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
      val tgt = 0
      val dmg = 1
      val zmb = 2
      val loader = 3   

      fun print_and_cat' [] = []
        | print_and_cat' ((lbl, asm)::rest) = let
            val _ = print (lbl ^ ": " ^ (is (length asm)) ^ "\n")
          in
            asm @ (print_and_cat' rest)
          end

      fun print_and_cat x = let 
            val result = print_and_cat' x
            val _ = print ("TOTAL: " ^ (is (length result)) ^ "\n\n")
          in
            result
           end
 
      val result = ref (
        print_and_cat [
        ("10k", fastnum dmg 10000),
        ("dblshot", fast_doubleshot 10 dmg 4 5 0),
        ("loadctr", fastnum tgt 0),
        ("zombie", zombie tgt dmg zmb),
        ("loader", zombie_loader zmb loader),
        ("bangbang", (rep 128 (apply loader Z @ succ tgt)))])
     
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
