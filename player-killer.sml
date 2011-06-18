structure Killer :> LAYER =
struct
  structure GS = GameState
  structure K = Kompiler
  datatype src = datatype K.src
  datatype card = datatype Card.card

  val L = LTG.LeftApply
  val R = LTG.RightApply  

  fun init gs = ()

  infix 9 --
  val op -- = Apply
  val $ = Var
  fun \ x exp = Lambda (x, exp)
  infixr 1 `
  fun a ` b = a b

  fun program {(* The slot that actually does the attacking *)
               attacker, 
               (* Slots used to hold parts of the program *)
               prog_slot, heal_slot, attack_slot, 
               (* Two slots that just store numbers *)
               damage_ref, heal_ref, 
               tmp_slot1, tmp_slot2} = let
    val heal1 = \ "victim" ` Card Help -- $"victim" -- Int attacker -- (Card Get -- Int damage_ref)
    val heal2 = \ "victim" ` (Card Help -- (Card Succ -- $"victim")
                                   -- Int attacker -- (Card Get -- Int 1))
    val heal = \ "victim" ` (Card S -- heal1 -- heal2) -- (Card Dbl -- $"victim")

    val attack = \ "victim" ` Card Attack -- Int attacker -- $"victim" -- (Card Get -- Int heal_ref)
    val zombiefunc = \ "unused" ` Card Attack -- $"slote" -- Int (255 - prog_slot) -- Int 10000
    val zombie = \ "slote" ` Card Zombie -- $"slote" -- zombiefunc
    val attackzomb = \ "slotf" ` (Card S -- attack -- zombie) -- $"slotf"
    val f = (Card S -- (Card Get -- Int heal_slot) -- (Card Get -- Int attack_slot))
    (* We run a loop over enemies *)
    val fixbigm = K.for_ref f prog_slot

    (* Do a little computation in tmps to confuse emenies. *)
    val p8 = (K.compile (Int 8192) tmp_slot1)
    val p0 = (K.compile (Card Get -- Int tmp_slot1) tmp_slot2)
    val p1 = (K.compile (Card Get -- Int tmp_slot2) damage_ref)

    val p7 = (K.compile (\ "x" ` Card Get -- Int prog_slot) attacker)
    val p2 = (K.compile (Int 12288) heal_ref)
    val phelp = (K.compile heal heal_slot)
    val pattack = (K.compile attackzomb attack_slot)
    val pmbig = (K.compile fixbigm 3)
    val pcompile = (K.compile (Card Get -- (Int 3)) prog_slot) @ 
                   [R (attacker, Attack)] @ (List.tabulate (128, fn x => R (prog_slot, Zero))) 
  in 
    p8 @ p0 @ p1 @ p2 @ phelp @ pattack @ pmbig @ pcompile
  end

  val turn = ref (program {attacker = 7, prog_slot = 6, heal_slot = 4, attack_slot = 5, 
                           tmp_slot1 = 0, tmp_slot2 = 8, damage_ref = 1, heal_ref = 2});
  val counter = ref 0;

  fun taketurn gs = 
  let
    fun f (t::ts) = (turn := ts; t)
      | f nil = L (I, 0)
    val (_, vits) = GS.myside gs
    fun showscores () = 
      if ((!counter) > 720) andalso ((!counter) < 800)
      then let
          val ballsmap = List.tabulate (8, fn x => (Int.toString (Array.sub (vits,x))))
        in
          eprint ("["^(String.concatWith "," ballsmap)^"]\n")
        end
      else ()
    val _ = counter := (!counter) + 1
  in
    f (!turn)
  end
end

 
structure Player = LayerFn(Killer)
