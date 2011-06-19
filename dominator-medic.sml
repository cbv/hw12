(* The medic looks for cells that are hurt or dead,
   and heals or revives them. Uses a strategy similar
   to the sniper. *)
structure Medic :> DOMINATOR =
struct
  structure GS = GameState
  datatype src = datatype Kompiler.src
  datatype dosturn = datatype DOS.dosturn

  structure EP = EmitProgram
  exception Medic of string

  infix 9 --
  val op -- = Apply
  val $ = Var
  fun \ x exp = Lambda (x, exp)
  infixr 1 `
  fun a ` b = a b

  val rtos = Real.fmt (StringCvt.FIX (SOME 2))
  val eprint = fn s => eprint ("[MEDIC] " ^ s ^ "\n")

  datatype mode =
      FindTarget
    | Loading of { status : EP.status ref,
                   child : DOS.pid,
                   myslot : int,
                   src : int,
                   (* How much does my program heal? *)
                   heal : int,
                   (* How much did health did the target have when
                      I selected it? *)
                   old_health : int,
                   target : int }
    | Healing of { myslot : int, src : int,
                   (* How many times have I already healed? *)
                   heals : int ref,
                   heal : int, old_health : int, target : int }

  (* Score my own slots for healing. These slots are prioritized
     using the same heuristics as sniper, but dead slots are treated
     as high-scoring, since we often want to revive them.

     TODO: Allow programs to actually advise on the importance
     of slots, which should override use-based heuristics.

     TODO: If a slot is freed, might want to clear its score.

     TODO: Should prioritize slots that are source field for
     old medics, since that lets us use those old medics again.

     Also, this is bad for reasons
     *)
  fun scoremyslotforhealing dos my_side my_stats idx =
      let val s = LTG.statfor my_stats idx
      in
          (DOS.is_reserved idx,
           LTG.slotisdead my_side idx,
           Array.sub(#2 my_side, idx),
           idx,
           (real (LTG.stat_left_applications s) +
            real (LTG.stat_right_applications s) +
            LTG.stat_damage_done s +
            LTG.stat_healing_done s +
            real (LTG.stat_iterations s) +
            10.0 * real (LTG.stat_gotten s)))
      end

  (* Keep track of old medics we made. We prefer to reuse them
     then to build new ones. *)
  type oldmedic = { src : int, target : int, amount : int,
                    (* Expected program. If it's changed, then
                       we can't use it. *)
                    prog : LTG.value }
  val oldmedics = Array.array (256, NONE : oldmedic option)

  (* XXX: Should use number of bits, but this is close *)
  fun compare_idx (i, ii) = Int.compare (ii, i)

  fun is_critical (i, (resv, dead, vit, idx, score)) =
      (resv andalso dead) orelse
      (resv andalso vit < 10000)

  (* If one is dead, then it is lexicographically higher priority. *)
  (* XXX use first component!! *)
  fun compare_scores (s1, s2) =
      (* XXX hax using 0 as index. *)
      (case (is_critical (0, s1), is_critical (0, s2)) of
           (true, false) => GREATER
         | (false, true) => LESS
         | _ =>
            (case (s1, s2) of
                 ((_, true, _, _, _), (_, false, _, _, _)) => GREATER
               | ((_, false, _, _, _), (_, true, _, _, _)) => LESS
               | ((r, _, v, i, s), (rr, _, vv, ii, ss)) =>
                       (case Int.compare (v, vv) of
                            EQUAL =>
                                (case (r, rr) of
                                     (true, false) => GREATER
                                   | (false, true) => LESS
                                   | _ => 
                                         (* This is really only for the case that the cards have
                                            been completely unused. Heal cheaper cards since
                                            they make good sources and are easier to heal. *)
                                         (case Real.compare (ss, s) of
                                              EQUAL => compare_idx (i, ii)
                                            | neq => neq))
                          | LESS => GREATER
                          | GREATER => LESS)))

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

          val prog = Kompiler.rrs_ref helpy prog_slot
          (* val prog_indirect = Kompiler.run_and_return_self helpy *)

          val insns = Kompiler.compile prog prog_slot
      in
          eprint ("Program length: " ^ Int.toString (length insns));
          insns
      end handle (e as Kompiler.Kompiler s) =>
          let in
              eprint ("Kompilation failed: " ^ s);
              raise e
          end

    (* XXX set priority based on how much damage I've taken to
       valuable cards? *)
    fun preview dos = ()

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

    (* Returns the valid old medics (can be used this turn) and erases
       the ones that are totally screwed. *)
    fun fixup_old_medics gs =
        let
            val myside = GS.myside gs
            val old = ref nil
            fun onemedic i =
                case Array.sub (oldmedics, i) of
                    NONE => ()
                  | SOME (oldmedic as { src, prog, amount, target }) =>
                if Array.sub (#1 myside, i) = prog orelse
                   (* XXX Can't use it. But maybe shouldn't erase it just for
                      being dead? We might revive it. *)
                   Array.sub (#2 myside, i) <= 0
                then 
                    if Array.sub (#2 myside, src) >= amount
                    then old := (i, oldmedic) :: !old
                    else ()
                else 
                    let in 
                        eprint ("An elder is no longer with us: " ^
                                Int.toString i ^ "(" ^ Int.toString src ^
                                "->" ^ Int.toString target ^
                                " @ " ^ Int.toString amount);
                        Array.update (oldmedics, i, NONE)
                    end
        in
            Util.for 0 255 onemedic;
            !old
        end

    (* Get a medic task, which consists of a source, target, and amount. *)
    datatype task = 
        MakeNewMedic of { src : int, target : int, stat : EP.status ref,
                          pid : DOS.pid, actual : int, amount : int }
      | UseOldMedic of { oldmedic : int, src : int, target : int, amount : int }

    fun gettask dos prog_slot : task =
        let
            val gs = DOS.gamestate dos
            (* first, see what old medics are still valid, and count them up. *)
            val valid_medics = fixup_old_medics gs
            (* val num_valid_medics = length valid_medics *)
            val () = eprint ("There are " ^ Int.toString (length valid_medics) ^
                             " valid oldmedics:\n  " ^
                             StringUtil.delimit "\n  "
                             (map (fn (i, { target, src, amount, ... }) =>
                                   Int.toString src ^ " -> " ^
                                   Int.toString target ^ " @ " ^
                                   Int.toString amount ^ " (in slot " ^
                                   Int.toString i ^ ")") valid_medics))
                             
            val myside = GS.myside gs
            val mystats = GS.mystats gs
                
            (* Find a high value slot on my own side to heal. *)
            val slots = List.tabulate (256, fn i =>
                                       (i, scoremyslotforhealing dos myside mystats i))

            val critical_slots = List.filter is_critical slots
        in
            if List.null critical_slots
            then ()
            else eprint ("There are " ^ Int.toString (length critical_slots) ^
                         " critical slots: " ^
                         StringUtil.delimit ", " (map (fn (i, _) => Int.toString i) critical_slots));

            (* if we have an old medic for a critical slot, use it. *)
            case ListUtil.findpartial (fn (i, _) =>
                                       ListUtil.findpartial
                                       (fn (om as (_, { target, ... })) =>
                                        if i = target
                                        then SOME om
                                        else NONE) valid_medics) critical_slots of
                SOME (oldmedic, { src, target, amount, ... }) =>
                    let in
                        eprint ("I have an old medic that can heal the critical " ^
                                "target " ^ Int.toString target ^ "!");
                        UseOldMedic { oldmedic = oldmedic, src = src, target = target,
                                      amount = amount }
                    end
              | NONE => 
              let
                  (* Old approach! *)

                  (* Find an easily-addressed slot with health,
                     which will be the source for healing. Get
                     the health amount we'll try to use. *)
                  val (src, heal, actual_src_health) = findsource myside

                  (* XXXX if I already have a valid oldmedic for this target, don't build another! 
                     Just use the one I have. *)

                  (* Maybe should have a lower bound on what it will
                     consider valuable, and back off if there are
                     no current high-value targets? *)
                  val (best, score) = ListUtil.max (ListUtil.bysecond compare_scores) slots
                  val () =
                      let val (resv, dead, vit, idx, score) = score
                      in eprint ("Best is" ^ (if resv then " resv" else "") ^
                                 (if dead then " dead" else "") ^
                                 " vit: " ^ Int.toString vit ^
                                 " idx: " ^ Int.toString idx ^
                                 " score: " ^ rtos score)
                      end

                  val prog = healprogram { src = src, amount = heal, target = best } prog_slot

                  (* Save child pid. If our program dies, we just kill
                     the child and try again, on the assumption that
                     there are no other medics. *)
                  val (stat, child_pid) = EP.emitspawn dos prog
              in
                  eprint ("New medic task: " ^ Int.toString src ^ " -> " ^
                          Int.toString best ^ " @ " ^ Int.toString heal ^
                          " in slot " ^ Int.toString prog_slot ^
                          ". Program length: " ^ Int.toString (length prog));

                  MakeNewMedic { src = src, target = best, stat = stat, pid = child_pid,
                                 actual = actual_src_health, amount = heal }
              end
        end

    val mode = ref FindTarget
    fun taketurn dos =
        let val gs = DOS.gamestate dos
        in
            case !mode of
                FindTarget =>
                 (case DOS.reserve_slot dos of
                      NONE => DOS.Can'tRun
                    | SOME prog_slot =>
                   (case gettask dos prog_slot of
                        MakeNewMedic { src : int, target : int, stat : EP.status ref,
                                       amount : int,
                                       pid : DOS.pid, actual : int } =>     
                        let in
                            mode := Loading { myslot = prog_slot,
                                              child = pid,
                                              target = target,
                                              src = src,
                                              old_health = actual,
                                              heal = amount,
                                              status = stat };
                            Can'tRun
                        end
                  | UseOldMedic { oldmedic : int, src : int, target : int, amount : int } =>
                        if DOS.reserve_fixed_slot dos oldmedic
                        then 
                            let in
                                mode := Healing { heals = ref 0,
                                                  myslot = oldmedic,
                                                  target = target,
                                                  src = src,
                                                  heal = amount,
                                                  (* XXX hax *)
                                                  old_health = amount };
                                taketurn dos
                            end
                        else
                          let in
                              eprint ("Ugh, can't reserve old medic #" ^
                                      Int.toString oldmedic ^
                                      ", that I would have used. " ^
                                      "Blanking and skipping for correctness.");
                              Array.update (oldmedics, oldmedic, NONE);
                              taketurn dos
                          end))

              | Loading { status = ref (EP.Progress _), ... } => Can'tRun
              | Loading { status = ref EP.Done, myslot, target, src,
                          heal, old_health, child = _ } =>
                       let in
                           mode := Healing { heals = ref 0,
                                             myslot = myslot,
                                             target = target,
                                             src = src,
                                             heal = heal,
                                             old_health = old_health };
                           taketurn dos
                       end

              | Healing { myslot, target, src, heals, heal, old_health } => 
                 let
                     val myside = GS.myside gs
                     val health = Array.sub (#2 myside, target)
                     val src_health = Array.sub (#2 myside, src)
                 in
                     (* Remember that we have a medic here. *)
                     Array.update (oldmedics, myslot,
                                   SOME { prog = Array.sub (#1 myside, myslot),
                                          src = src,
                                          target = target,
                                          amount = heal });

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
              | Loading { status = ref (EP.Paused _), child, myslot, ... } =>
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

