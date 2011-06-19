
(* current story: broken; attacks opponent's slot 0 once and zombies it. Working on making the zombie do something. *)

val print = EPrint.eprint

structure Gwillen8Or :> LAYER =
struct
  structure GS = GameState

(*  open Kompiler *)
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
  fun zombie tgt_slot dmg_slot dem_slot daemon_code zombie_code = let val exp =
      \"_" ` \"_" ` (seq
          (Card Zombie -- Int dem_slot
                       -- ((Card Copy -- Int daemon_code) -- (Card Succ -- (Card Get -- Int tgt_slot))))
          (Card Help -- (Card Dbl -- (Card Get -- Int tgt_slot))
                     -- (Card Succ -- (Card Dbl -- (Card Get -- Int tgt_slot)))
                     -- (Card Copy -- Int dmg_slot))) 
    in
      Kompiler.compile exp zombie_code
    end

  fun init _ = let
        val _ = print "HELLO WORLD\n"
        val exp = [] (*zombie_help_0_0_copy_0 1*)
        val exp = zombie 0 1 0 0 2
        val _ = print ((LTG.turns2str exp) ^ "\n")
     in
        ()
     end

  val _ = init ()


  (* Zombie performs \_ . Zombie daemon_install (Copy daemon_code) *)
  (* daemon_install is the number, in the _enemy's_ numbering system, of the slot on _our_ side where the daemon will be installed. daemon_code is the number, in _our_ numbering system, of the slot on _our_ side where the daemon program is kept. *)
  (* 1 0 0 2 *)
(*
  fun zombie dmg_slot dem_slot daemon_code zombie_code = let val exp =
      \"slot" ` \"_" ` (seq
          (Card Zombie -- Int dem_slot
                       -- ((Card Copy -- Int daemon_code) -- (Card Succ -- $"slot")))
          (Card Help -- (Card Dbl -- $"slot")
                     -- (Card Succ -- (Card Dbl -- $"slot"))
                     -- (Card Copy -- Int dmg_slot))) 
    in
      Kompiler.compile exp zombie_code
    end

*)
  (* Daemon performs \_ . Zombie zombie_install (Get zombie_code) *)
  (* zombie_install is the number, in _our_ numbering system, of the slot on _their_ side where the zombie will be installed. zombie_code is the number, in _our_ numbering system, of the slot on _our_ side where the zombie program is kept. *)
  fun daemon zombie_install zombie_code daemon_code = let
      val exp = \"slot" ` \"_" ` Card Zombie -- Int zombie_install 
                                             -- ((Card Get -- Int zombie_code) -- $"slot")
    in
      Kompiler.compile exp daemon_code
    end

  val is = Int.toString

  (* help 255 0 10000 ; assumes 10000 is in slot dmg ; uses slot scratch which must be I *)
  fun help_me dmg fromslot scratch = fastload scratch Help @
                    apply_slot_to_slot scratch fromslot @
                    apply scratch Z @
                    apply_slot_to_slot scratch dmg
                         
  fun shootem (dmg_cell : int) (tgt : int)
    (mine1 : int) (scratch2 : int) : LTG.turn list =
      clear scratch2 @
      fastload scratch2 Attack @
      apply_slot_to_int scratch2 mine1 @
      apply_slot_to_int scratch2 tgt @
      apply_slot_to_slot scratch2 dmg_cell (* executes Attack minescratch2 enemy
      8292 *)

  val strategy = let
      val daemon_code = 0
      val endslot = 3 (* number of the end slot, i.e. 255 *)
      val dmg = 1
      val zombie_code = 2
      val dmg_back = 4
      val loader = 5   
      val scratch_slot = 6
      val tgt = 0

      val daemon_install = 0 (* our slot 255, in enemy's numbering system *)
      val zombie_install = 255 (* their slot zero, in our numbering system *)
    
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
        ("255", fastnum endslot 255),
        ("10k", fastnum dmg 10000),
        ("helpme", help_me dmg endslot scratch_slot),
        ("dmg_back", slownum dmg_back dmg @ [L Get dmg_back]),
        ("dbl dmg", dbl dmg_back),
        ("shootem", shootem dmg_back 0 0 scratch_slot),
        ("zombie", zombie tgt dmg 0 daemon_code zombie_code),
        ("daemon", daemon 0 zombie_code daemon_code),
        ("loadctr", fastnum tgt 0),
        ("launch", slownum scratch_slot daemon_code @ [L Get scratch_slot] @ [R scratch_slot Z] @ [R scratch_slot I]),
        ("c&c", rep 128 (succ tgt))
      ])

(*
      val _ = print ("255: " ^ (is (length make_255)) ^
                     " hit: " ^ (is (length hitthem)) ^ 
                     " 10k: " ^ (is (length make_10000)) ^ 
                     " help: " ^ (is (length helpme)) ^ 
                     " copydam: " ^ (is (length copydam)) ^
                     " dam2: " ^ (is (length dam2)) ^ 
                     " zom: " ^ (is (length zom)) ^ 
                     " go: " ^ (is (length go)) ^ "\n")
*)
     
    in
      result
    end


  fun taketurn _ = case (!strategy) of nil => L I 0
    | today::future => let 
      val _ = strategy := future
    in
      today
    end

end

structure Player = LayerFn(Gwillen8Or)
