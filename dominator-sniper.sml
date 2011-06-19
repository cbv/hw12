(* Note, I started using "sniper" again.
   This should be better than "ripens". -tom7 *)
structure Sniper :> DOMINATOR =
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

  datatype mode =
      (* First need to create the gun program, which never changes. *)
      CreateGun
      (* Once the program is in place, find a target and put it in target_slot. *)
    | BuildingGun of { status : EP.status ref, gun_slot : int, target_slot : int,
                       src_slot : int }
      (* We loop between the following two for the rest of time. We could consider
         making a new gun, but the current strategy is to hope for the medic to
         save us.

         We find the target and source at the same time, which is definitely suboptimal.
         Targeting after choosing a source allows us to have lower latency on assassinating
         small programs that get hot. Setting source afte choosing a target reduces the
         risk that the opponent harms our source. *)
    | ReTarget of { gun_slot : int, target_slot : int, src_slot : int }
      (* Now keep attacking until it's dead, or something happens to our
         equipment. *)
    | Attacking of { status : EP.status ref,
                     shots : int ref,
                     (* Slot containing the index of the source for attack power,
                        which is referred to by the gun. *)
                     src_slot : int,
                     (* Slot containing the gun. *)
                     gun_slot : int,
                     (* Slot containing the index of
                        the target, which is referred
                        to by the gun. *)
                     target_slot : int }

  val compare_scores = ListUtil.bysecond Real.compare

  val lastmsg = ref ""
  val eprint =
      fn s => if s = !lastmsg
              then ()
              else (eprint ("[SNIPER] " ^ s ^ "\n"); lastmsg := s)

  (* Source must have this much health to be selected *)
  val CONSERVATIVE_HEALTH_NEEDED = 10000
  val ACTUAL_HEALTH_NEEDED = 8193
  (* Start by self-healing by 8192 this many times. This
     allows me to do two attacks at 8192 without
     losing any health, which usually kills in one turn. *)
  val SELF_HEALING_ITERATIONS = 21

  val rtos = Real.fmt (StringCvt.FIX (SOME 2))
      
  fun create () =
  let

    (* Just for accounting / tuning. *)
    val total_targeting = ref 0.0
    val num_targetings = ref 0.0

    (* Maybe should have a lower bound on what it will
       consider valuable, and reduce priority if there
       are no current high-value targets. *)

    (* Makes a program that attacks the target slot index,
       and then returns itself (so it sticks around). *)
    fun attackprogram target_slot src_slot prog_slot =
      let
          fun repeat 1 e = e
            | repeat n e = repeat (n - 1) e -- e

          val gettarg = Card LTG.Get -- Int target_slot
          val getsrc = Card LTG.Get -- Int src_slot

          val dec =
              (\"target" `
               (* empirical. Attack is MUCH more efficient.
                  Should use attack. *)
               repeat 100 (Card LTG.Dec -- $"target")) --
              (Card LTG.Get -- Int target_slot)
          
          val chargedattack =
              (\"src" `
               \"i8192" `
               (* Charge up *)
               repeat SELF_HEALING_ITERATIONS (Card LTG.Help -- $"src" -- $"src" -- $"i8192") --
               (* Then attack exactly twice. *)
               (Card LTG.Attack -- $"src" -- gettarg -- $"i8192") --
               (Card LTG.Attack -- $"src" -- gettarg -- $"i8192")) --
              getsrc -- Int 8192

          val prog = Kompiler.run_and_return_self chargedattack
          val () = eprint (Kompiler.src2str prog)
          val insns = Kompiler.compile_never_exponential prog prog_slot
      in
          eprint ("Compiled to : " ^ Int.toString (length insns));
          insns
      end handle (e as Kompiler.Kompiler s) =>
          let in
              eprint ("Kompilation failed: " ^ s ^ "\n");
              raise e
          end

    fun preview dos = ()

    (* This is just for diagnostics. *)
    val was_stuck = ref false

    fun array_sub (a, x) =
        Array.sub (a, x) handle Subscript =>
            let in
                eprint ("BAD SUB " ^ Int.toString x);
                raise Subscript
            end

    val all256 = List.tabulate (256, fn i => i)
    fun getbestsource src_slot myside =
        case List.mapPartial (fn i => let val vit = array_sub(#2 myside, i)
                                      in if vit < CONSERVATIVE_HEALTH_NEEDED
                                         then NONE
                                         else SOME (i, vit)
                                      end) all256 of
            (* XXX could blend amount of health with index bits.
               Should prefer things with short edit distance from
               current contents of src_slot. *)
            (i, _) :: _ => SOME i
          | nil => NONE

    (* cellidx is the index of a cell on my side that should
       contain a number. get that number or NONE of something
       is wrong. *)
    fun getindirectint gs cellidx =
        if cellidx > 256
        then (eprint ("BAD IDX " ^ Int.toString cellidx); NONE)
        else
        let
            val myside = GS.myside gs
        in
            (case array_sub (#1 myside, cellidx) of
                 LTG.VInt i => SOME i
               | _ => NONE)
        end

    (* This seems to work now, but we should really prefer short distances that use
       succ vs. short distances that use double, because double gets us to large
       numbers, which are then hard to reuse. *)
    fun getdistancefnfortargeting gs slot =
        (case getindirectint gs slot of
             NONE => (fn i => Numbers.naive_cost (255 - i))
           | SOME given =>
             (fn i => length (Numbers.convert_from { given = given, desired = 255 - i })))

    (* Put the number in the slot, but if the slot already has something useful
       for computing it (specifically, some smaller number), start with that. *)
    fun putnuminslot gs num slot =
        case getindirectint gs slot of
            NONE => Kompiler.compile (Int num) slot
          | SOME given => 
                let 
                    (* val () = eprint (Int.toString given ^ " -> " ^ Int.toString num ^ "? ") *)
                    val p = map (LTG.halfturn2turn slot) (Numbers.convert_from { given = given,
                                                                                 desired = num })

                in
                    (* eprint ("  ... takes " ^ Int.toString (length p)); *)
                    p
                end

    fun getindirectidx gs cellidx =
        case getindirectint gs cellidx of
            NONE => NONE
          | SOME i => if i > 255
                      then NONE
                      else SOME i

    val mode = ref CreateGun
    fun taketurn dos =
        let val gs = DOS.gamestate dos
        in
            case !mode of
                CreateGun =>
                 (case (DOS.reserve_slot dos, 
                        DOS.reserve_addressable_slot dos,
                        DOS.reserve_addressable_slot dos) of
                   (SOME gun_slot, SOME target_slot, SOME src_slot) =>
                   let
                     val prog = attackprogram target_slot src_slot gun_slot
                     val (stat, child_pid) = EP.emitspawn dos prog
                   in
                     eprint ("Assembling gun in: " ^ Int.toString gun_slot ^
                             " reading target from: " ^ Int.toString target_slot ^
                             " and src from: " ^ Int.toString src_slot ^
                             " Program length: " ^ Int.toString (length prog));
                     mode := BuildingGun { status = stat, gun_slot = gun_slot,
                                           target_slot = target_slot,
                                           src_slot = src_slot };
                     was_stuck := false;
                     taketurn dos
                   end
                  | _ => 
                     let in 
                         eprint ("Sniper can't get slots.");
                         DOS.release_all_slots dos; 
                         DOS.Can'tRun
                     end)

              | BuildingGun { status = ref (EP.Progress _), ... } => DOS.Can'tRun
              (* Hope that medic helps us. *)
              | BuildingGun { status = ref (EP.Paused _), ... } => DOS.Can'tRun
              | BuildingGun { status = ref EP.Done, gun_slot, target_slot, src_slot } =>
                   let in
                       eprint ("Gun is assembled :D");
                       mode := ReTarget { gun_slot = gun_slot, 
                                          target_slot = target_slot,
                                          src_slot = src_slot };
                       was_stuck := false;
                       taketurn dos
                   end

              | ReTarget { gun_slot, target_slot, src_slot } =>
                   (* Check if any of our slots are dead. If so,
                      yield to medic. This would be a good place to
                      give hints to the medic! *)
                   if LTG.slotisdead (GS.myside gs) gun_slot orelse
                      LTG.slotisdead (GS.myside gs) target_slot orelse
                      LTG.slotisdead (GS.myside gs) src_slot
                   then
                       let in
                           if !was_stuck
                           then eprint ("Gun/target/src slots are dead!")
                           else ();
                           was_stuck := true;
                           Can'tRun
                       end
                   else
                   (case getbestsource src_slot (GS.myside gs) of
                        NONE =>
                            let in
                                eprint ("There are no valid sources for the " ^
                                        "sniper. MEDIC!");
                                Can'tRun
                            end
                      | SOME best_src => 
                   let
                     (* Prefer things that we can emit easily, because they're
                        close to our existing target slot contents. *)
                     val distancefn = getdistancefnfortargeting gs target_slot

                     val slots = List.tabulate 
                         (256, fn i =>
                          (i, GS.scoreopponentslot_withdistance gs distancefn i))

                     (* Maybe should have a lower bound on what it will
                        consider valuable, and just heal/revive if there
                        are no current high-value targets. *)
                     val (best_target, _) = ListUtil.max compare_scores slots

                     (* Put the number in our targeting slot. *)
                     (* XXX use spoons's program *)
                     val prog = 
                         putnuminslot gs (255 - best_target) target_slot @
                         putnuminslot gs best_src src_slot

                     (* Ignore child pid since we never kill it. *)
                     val (stat, child_pid) = EP.emitspawn dos prog

                     val proglen = length prog
                   in
                     num_targetings := !num_targetings + 1.0;
                     total_targeting := !total_targeting + real proglen;
                     eprint ("Retarget " ^ Int.toString best_src ^ " -> " ^
                             Int.toString best_target ^ " in " ^
                             Int.toString proglen ^ " (avg " ^
                             rtos (!total_targeting / !num_targetings) ^ ")");
                     mode := Attacking { shots = ref 0,
                                         gun_slot = gun_slot,
                                         target_slot = target_slot,
                                         src_slot = src_slot,
                                         status = stat };
                     was_stuck := false;
                     Can'tRun
                   end)

              | Attacking { status = ref (EP.Progress _), ... } =>
                      let in
                          if !was_stuck
                          then eprint ("Unstuck on retargeting! Thanks!")
                          else ();
                          was_stuck := false;
                          Can'tRun
                      end

              | Attacking { status = ref EP.Done, shots, gun_slot, target_slot,
                            src_slot, ... } =>
                 if LTG.slotisdead (GS.myside gs) gun_slot orelse
                    LTG.slotisdead (GS.myside gs) target_slot orelse
                    LTG.slotisdead (GS.myside gs) src_slot
                 then
                     let in
                          if !was_stuck
                          then eprint ("Sniper's gun/target/src slot " ^
                                       Int.toString gun_slot ^ "/" ^
                                       Int.toString target_slot ^ "/" ^
                                       Int.toString src_slot ^
                                       " was killed! Hoping for medic.\n")
                          else ();
                          was_stuck := true;
                          Can'tRun
                     end
                 else
                   (case (getindirectint gs src_slot, getindirectint gs target_slot) of
                      (SOME srcidx, SOME targidx) =>
                     let 
                         (* Because it's the opponent, the value stored in the
                            slot is actually 255 - the target's real index. *)
                         val targidx = 255 - targidx

                         val myside = GS.myside gs
                         val srchealth = array_sub (#2 myside, srcidx)
                         
                         val theirside = GS.theirside gs
                         val targhealth = array_sub (#2 theirside, targidx)
                         val num = array_sub (#1 (GS.myside gs), target_slot)
                     in
                         was_stuck := false;
                         if targhealth <= 0
                         then
                             let in
                                 eprint ("Success! Killed slot " ^
                                         Int.toString targidx ^
                                         " in " ^ Int.toString (!shots) ^ " shot(s).");
                                 mode := ReTarget { gun_slot = gun_slot,
                                                    src_slot = src_slot,
                                                    target_slot = target_slot };
                                 taketurn dos
                             end
                         else
                             if srchealth < ACTUAL_HEALTH_NEEDED
                             then
                               let in
                                   eprint ("Source health fell to " ^
                                           Int.toString srchealth ^
                                           ". Retargeting.");
                                   mode := ReTarget { gun_slot = gun_slot,
                                                      src_slot = src_slot,
                                                      target_slot = target_slot };
                                   taketurn dos
                               end
                             else
                             let in
                                 (* Otherwise keep attacking. *)
                                 (* eprint ("Attack!"); *)
                                 shots := !shots + 1;
                                 DOS.Turn (LTG.RightApply (gun_slot, LTG.I))
                             end
                     end
                    | _ =>
                     let in
                         eprint ("src/target cells don't even contain ints. retargeting.");
                         mode := ReTarget { gun_slot = gun_slot, target_slot = target_slot,
                                            src_slot = src_slot };
                         taketurn dos
                     end)

              (* Optimistically hope that medic will heal it? *)
              | Attacking { status = ref (EP.Paused i), ... } =>
                 let in
                     if !was_stuck
                     then ()
                     else eprint ("Stuck because slot " ^ Int.toString i ^ " is dead.");
                     was_stuck := true;
                     Can'tRun
                 end
        end
  in
    { preview = preview,
      taketurn = taketurn }
  end
end

