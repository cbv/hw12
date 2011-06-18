
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


(*
  (* Zombie performs \_.Help (2*(Copy tgt)) (2(Copy tgt)+1) (Copy dmg) *)
  fun zombie tgt dmg slot = let
      val exp = \"_" ` Card Help -- (Card Dbl -- (Card Copy -- Int tgt)) -- (Card Succ -- (Card Dbl -- (Card Copy -- Int tgt))) -- (Card Copy -- Int dmg) 
    in
      (* this should be named like, compile_assume_identity *)
      Kompiler.compile_no_clear_rev exp slot
    end
*)

  (* WARINING WARNING WARINING: 255-x *)

  (* Zombie performs \_ . Zombie daemon_install (Copy daemon_code) *)
  (* daemon_install is the number, in the _enemy's_ numbering system, of the slot on _our_ side where the daemon will be installed. daemon_code is the number, in _our_ numbering system, of the slot on _our_ side where the daemon program is kept. *)
  fun zombie daemon_install daemon_code zombie_code = let
      val exp = \"_" ` Card Zombie -- Int daemon_install -- (Card Copy -- Int daemon_code)
    in
      Kompiler.compile_no_clear_rev exp zombie_code
    end

  (* Daemon performs \_ . Zombie zombie_install (Get zombie_code) *)
  (* zombie_install is the number, in _our_ numbering system, of the slot on _their_ side where the zombie will be installed. zombie_code is the number, in _our_ numbering system, of the slot on _our_ side where the zombie program is kept. *)
  fun daemon zombie_install zombie_code daemon_code = let
      val exp = \"_" ` Card Zombie -- Int zombie_install -- (Card Get -- Int zombie_code)
    in
      Kompiler.compile_no_clear_rev exp daemon_code
    end

  fun zombie_loader zombie_code loader_code zombie_install = Kompiler.compile_no_clear_rev (rrs_ref (
    Card Zombie -- Int zombie_install -- (Card Get -- Int zombie_code)
(*
      fastload s Zombie @
      apply s Z @
      apply_slot_to_slot s zombie) s
*)
  ) loader_code) loader_code

  val is = Int.toString

  (* help 255 0 10000 ; assumes 10000 is in slot dmg ; uses slot scratch which must be I *)
  fun help_me dmg fromslot scratch = fastload scratch Help @
                    apply_slot_to_slot scratch fromslot @
                    apply scratch Z @
                    apply_slot_to_slot scratch dmg
                         
  fun shootem (dmg_cell : int) (enemy_cell : int (*reg containing target*)) (mine1 : int) (scratch2 : int) : LTG.turn list =
      fastload scratch2 Attack @
      apply_slot_to_int scratch2 mine1 @
      apply_slot_to_slot scratch2 enemy_cell @
      apply_slot_to_slot scratch2 dmg_cell (* executes Attack minescratch2 enemy 8292 *)

  val strategy = let
      val tgt = 0
      val endslot = 1 (* number of the end slot, i.e. 255 *)
      val dmg = 2
      val zombie_code = 3
      val daemon_code = 4
      val loader = 5   
      val scratch_slot = 6
      val scratch_slot_2 = 7

      val daemon_install = 0 (* our slot 255, in enemy's numbering system *)
      val zombie_install = 255 (* their slot zero, in our numbering system *)

      val make_255 = fastnum endslot 255
      val hitthem = doubleshot (REG endslot) 0 1 scratch_slot scratch_slot_2
      val make_10000 = fastnum dmg 10000
      val helpme = help_me dmg endslot scratch_slot
      val zom = zombie daemon_install daemon_code zombie_code
      val dam = daemon zombie_install zombie_code daemon_code
      val copyloader = fastnum loader daemon_code @ [L Get loader]
      val go = apply loader Z
 
      val result = ref (
        fastnum endslot 255 @
        fastnum dmg 10000 @
        help_me dmg endslot scratch_slot @
        dbl dmg @
        shootem dmg endslot 0 scratch_slot @
        zombie daemon_install daemon_code zombie_code @
        daemon zombie_install zombie_code daemon_code @
        fastnum loader daemon_code @
        [L Get loader] @
        apply loader Z)

      val _ = print ("255: " ^ (is (length make_255)) ^
                     " hit: " ^ (is (length hitthem)) ^ 
                     " 10k: " ^ (is (length make_10000)) ^ 
                     " help: " ^ (is (length helpme)) ^ 
                     " zom: " ^ (is (length zom)) ^ 
                     " dam: " ^ (is (length dam)) ^ 
                     " copyloader: " ^ (is (length copyloader)) ^ 
                     " go: " ^ (is (length go)) ^ "\n")
     
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
