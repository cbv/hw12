(* Copied from Medic  *)
structure LazyMedic :> DOMINATOR =
struct
  structure GS = GameState
  datatype src = datatype Kompiler.src
  datatype dosturn = datatype DOS.dosturn

  structure EP = EmitProgram

  infix 9 --
  val op -- = Apply
  val $ = Var
  fun \ x exp = Lambda (x, exp)
  infixr 1 `
  fun a ` b = a b

  val eprint = fn s => eprint ("[Z-MEDIC] " ^ s ^ "\n")

  val initial_priority = ref ~1.0 (* will be set in first preview *)

  datatype mode =
      FindTarget
      (* Once the program is in place, revive and heal until
         we're pretty happy with its health. *)
    | Healing of { status : EP.status ref,
                   child : DOS.pid,
                   myslot : int,
                   src : int,
                   (* How many times have I already healed? *)
                   heals : int ref,
                   (* How much does my program heal? *)
                   heal : int,
                   (* How much did health did the target have when
                      I selected it? *)
                   old_health : int,
                   target : int }

  (* Score my own slots for healing. These slots are prioritized
     using the same heuristics as sniper, but dead slots are treated
     as high-scoring, since we often want to revive them.

     TODO: Allow programs to actually advise on the importance
     of slots, which should override use-based heuristics.

     TODO: If a slot is freed, might want to clear its score.

     Also, this is bad for reasons
     *)
  fun scoremyslotforhealing my_side my_stats idx =
      let val s = LTG.statfor my_stats idx
      in
          (LTG.slotisdead my_side idx,
           Array.sub(#2 my_side, idx),
           idx,
           (real (LTG.stat_left_applications s) +
            real (LTG.stat_right_applications s) +
            LTG.stat_damage_done s +
            LTG.stat_healing_done s +
            real (LTG.stat_iterations s) +
            real (LTG.stat_gotten s)))
      end

  (* XXX: Should use number of bits, but this is close *)
  fun compare_idx (i, ii) = Int.compare (ii, i)

  (* If one is dead, then it is lexicographically higher priority. *)
  fun compare_scores ((true, _, _, _), (false, _, _, _)) = GREATER
    | compare_scores ((false, _, _, _), (true, _, _, _)) = LESS
    | compare_scores ((_, v, i, s), (_, vv, ii, ss)) =
      (case Int.compare (v, vv) of
           EQUAL =>
               (* This is really only for the case that the cards have
                  been completely unused. Heal cheaper cards since
                  they make good sources and are easier to heal. *)
               (case Real.compare (ss, s) of
                    EQUAL => compare_idx (i, ii)
                  | neq => neq)
         | LESS => GREATER
         | GREATER => LESS)

  val HEALTH_GOAL = 20000

  fun compare_sources (_, vitality) (i, ii) =
    let
        val PLENTY_OF_HEALTH = HEALTH_GOAL div 4
        val v = Array.sub (vitality, i)
        val vv = Array.sub (vitality, ii)
    in
        (* If they both have plenty of health, then just take something
           with a small index. *)
        if v >= PLENTY_OF_HEALTH andalso vv >= PLENTY_OF_HEALTH
        then compare_idx (i, ii)
        else Int.compare (v, vv)
    end

  val all256 = List.tabulate (256, fn i => i)

  fun create () =
  let

    (* Maybe should have a lower bound on what it will
       consider valuable, and just heal/revive if there
       are no current high-value targets. *)

    (* Makes a program that revives, and heals the target from the
       source slot for the given health, then returns itself (so it sticks around). *)
    fun healprogram { src : int, amount : int, target : int } prog_slot =
      let
          val helpy =
              (\ "src" ` \ "target" ` \ "amount" `
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

          val prog = Kompiler.run_and_return_self helpy
      in
          Kompiler.compile prog prog_slot
      end handle (e as Kompiler.Kompiler s) =>
          let in
              eprint ("Kompilation failed: " ^ s);
              raise e
          end

    val mode = ref FindTarget

    fun preview dos =
        if !initial_priority < 0.0 then initial_priority := DOS.getpriority dos
        else 
          case !mode of 
            FindTarget => ()
          | Healing { target, ... } =>
               (* If the target is allocated then run with our usual priority. *)
            if DOS.is_reserved target then
              (eprint ("");
               DOS.setpriority (DOS.getpid dos) (!initial_priority))
            else (* Otherwise run at low priority. *)
              (eprint ("");
               DOS.setpriority (DOS.getpid dos) (!initial_priority / 2.0))

    (* How much health do we expect to have in the source slot
       by the time we actually do the healing? Set dynamically
       by observing what happened in previous attempts. Never
       more than 1.0, never less than 0.25 *)
    val discount_estimate = ref 1.0
    fun update_discount_estimate (actual, expected) =
        let
            (* XXX: time weighted average? *)
            val de = real actual / real expected
            val newde =
                if !discount_estimate < de
                then (!discount_estimate + de) * 0.5
                else de

            val newde = if newde > 1.0
                        then 1.0
                        else if newde < 0.5
                             then 0.5
                             else newde
        in
            discount_estimate := newde
        end

    (* Only situation in which there is no source is if all our
       slots are dead, in which case too bad! *)
    fun findsource (myside as (_, vitality)) =
      let
          val slots = all256
          val best : int = ListUtil.max (compare_sources myside) slots
          val actual_health = Array.sub (vitality, best)
          val discounted_health = Real.trunc (real actual_health * !discount_estimate)
          val heal_health = Numbers.next_lowest_power_of_two discounted_health
      in
          (best, heal_health, actual_health)
      end

    fun taketurn dos =
        let val gs = DOS.gamestate dos
        in
            case !mode of
                FindTarget =>
                 (case DOS.reserve_addressable_slot dos of
                      NONE => DOS.Can'tRun
                    | SOME prog_slot =>
                   let
                     (* Find an easily-addressed slot with health,
                        which will be the source for healing. Get
                        the health amount we'll try to use. *)
                     val myside = GS.myside gs
                     val (src, heal, actual_src_health) = findsource myside

                     (* Find a high value slot on my own side to heal. *)
                     val mystats = GS.mystats gs
                     val slots = List.tabulate (256, fn i =>
                                                (i, scoremyslotforhealing myside mystats i))

                     (* Maybe should have a lower bound on what it will
                        consider valuable, and just heal/revive if there
                        are no current high-value targets. *)
                     val (best, _) = ListUtil.max (ListUtil.bysecond compare_scores) slots

                     val prog = healprogram { src = src, amount = heal, target = best } prog_slot

                     (* Save child pid. If our program dies, we just kill
                        the child and try again, on the assumption that
                        there are no other medics. *)
                     val (stat, child_pid) = EP.emitspawn dos prog
                   in
                     eprint ("New task: " ^ Int.toString src ^ " -> " ^
                             Int.toString best ^ " @ " ^ Int.toString heal ^
                             ". Program length: " ^ Int.toString (length prog));

                     mode := Healing { myslot = prog_slot,
                                       child = child_pid,
                                       target = best,
                                       src = src,
                                       heals = ref 0,
                                       old_health = actual_src_health,
                                       heal = heal,
                                       status = stat };
                     Can'tRun
                   end)
              | Healing { status = ref (EP.Progress _), ... } => Can'tRun
              | Healing { status = ref EP.Done, myslot, target, src, heals,
                          heal, old_health, child = _ } =>
                 let
                     val myside = GS.myside gs
                     val health = Array.sub (#2 myside, target)
                     val src_health = Array.sub (#2 myside, src)
                 in
                     if !heals = 0
                     then update_discount_estimate (src_health, old_health)
                     else ();

                     (* If we don't have enough health in the source, then
                        we're going to fail. *)
                     if src_health < heal
                     then
                         let in
                             (* XXX if the target is dead now, we should
                                still run our program once, since it revives. *)
                             eprint ("Source doesn't have " ^ Int.toString heal ^
                                     " health to heal with.");
                             DOS.release_slot dos myslot;
                             mode := FindTarget;
                             taketurn dos
                         end
                     else
                         if health >= HEALTH_GOAL
                         then
                             let in
                                 eprint ("Success! Healed slot " ^
                                         Int.toString target ^ " " ^
                                         Int.toString (!heals) ^
                                         " times to bring it to " ^
                                         Int.toString health ^ ".");
                                 DOS.release_slot dos myslot;
                                 mode := FindTarget;
                                 taketurn dos
                             end
                         else
                             let in
                                 (* Otherwise keep healing. *)
                                 heals := !heals + 1;
                                 DOS.Turn (LTG.RightApply (myslot, LTG.I))
                             end
                 end

              (* This is pretty bad news, since nobody will be around to
                 heal it. We'll kill the program and try again... *)
              | Healing { status = ref (EP.Paused _), child, myslot, ... } =>
                 let in
                     eprint ("Medic: My child EmitProgram was interrupted. Killing.");
                     DOS.kill child;
                     (* This slot is useless to me now, since it's dead. *)
                     DOS.release_slot dos myslot;
                     mode := FindTarget;
                     taketurn dos
                 end
        end
  in
    { preview = preview,
      taketurn = taketurn }
  end
end

