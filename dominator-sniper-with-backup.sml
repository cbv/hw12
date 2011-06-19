(* Note, I started using "sniper" again.
   This should be better than "ripens". -tom7 *)
structure SniperWithBackup :> DOMINATOR =
struct
  structure GS = GameState
  datatype src = datatype Kompiler.src
  datatype dosturn = datatype DOS.dosturn

  structure EP = EmitProgram
  
  structure B = Backup

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
    | BuildingGun of { status : EP.status ref, gun_slot : int, target_slot : int }
      (* We loop between the following two for the rest of time. We could consider
         making a new gun, but the current strategy is to hope for the medic to
         save us. *)
    | FindTarget of { gun_slot : int, target_slot : int, backup : unit B.status ref }
      (* Now keep attacking until it's dead, or something happens to our
         equipment. *)
    | Attacking of { status : EP.status ref,
                     shots : int ref,
                     (* Slot containing the gun. *)
                     gun_slot : int,
                     (* Slot containing the index of
                        the target, which is referred
                        to by gun_slot. *)
                     target_slot : int,
                     target : int,
                     backup : unit B.status ref }

  val compare_scores = ListUtil.bysecond Real.compare

  val lastmsg = ref ""
  val eprint =
      fn s => if s = !lastmsg
              then ()
              else (eprint ("[SNIPER] " ^ s ^ "\n"); lastmsg := s)

  fun create () =
  let

    (* Maybe should have a lower bound on what it will
       consider valuable, and reduce priority if there
       are no current high-value targets. *)

    (* Makes a program that attacks the target slot index,
       and then returns itself (so it sticks around). *)
    fun attackprogram target_slot prog_slot =
      let
          fun repeat 1 e = e
            | repeat n e = repeat (n - 1) e -- e

          val dec =
              (\"target" `
               (* empirical. Attack is MUCH more efficient.
                  Should use attack. *)
               repeat 100 (Card LTG.Dec -- $"target")) --
              (Card LTG.Get -- Int target_slot)

          val prog = Kompiler.run_and_return_self dec
      in
          eprint (Kompiler.src2str prog);
          Kompiler.compile prog prog_slot
      end handle (e as Kompiler.Kompiler s) =>
          let in
              eprint ("Kompilation failed: " ^ s ^ "\n");
              raise e
          end

    fun preview dos = ()

    (* This is just for diagnostics. *)
    val was_stuck = ref false

    val mode = ref CreateGun
    fun taketurn dos =
        let val gs = DOS.gamestate dos
        in
            case !mode of
                CreateGun =>
                 (case (DOS.reserve_slot dos, DOS.reserve_addressable_slot dos) of
                    (NONE, NONE) => DOS.Can'tRun
                  | (NONE, SOME s) => (DOS.release_slot dos s; DOS.Can'tRun)
                  | (SOME s, NONE) => (DOS.release_slot dos s; DOS.Can'tRun)
                  | (SOME gun_slot, SOME target_slot) =>
                   let
                     val prog = attackprogram target_slot gun_slot
                     val (stat, child_pid) = EP.emitspawn dos prog
                   in
                     eprint ("Assembling gun in: " ^ Int.toString gun_slot ^
                             " reading from: " ^ Int.toString target_slot ^
                             " Program length: " ^ Int.toString (length prog));
                     mode := BuildingGun { status = stat, gun_slot = gun_slot,
                                           target_slot = target_slot };
                     was_stuck := false;
                     taketurn dos
                   end)

              | BuildingGun { status = ref (EP.Progress _), ... } => DOS.Can'tRun
              (* Hope that medic helps us. *)
              | BuildingGun { status = ref (EP.Paused _), ... } => DOS.Can'tRun
              | BuildingGun { status = ref EP.Done, gun_slot, target_slot } =>
                   let 
                       val (bstat, backup_pid) = B.backupspawn dos { src = gun_slot, use_addressable = false, done_callback = fn () => () }
                   in
                       eprint ("Gun is assembled :D");
                       mode := FindTarget { gun_slot = gun_slot, target_slot = target_slot, backup = bstat };
                       was_stuck := false;
                       taketurn dos
                   end
              | FindTarget { gun_slot, target_slot, backup } =>
                   if LTG.slotisdead (GS.myside gs) gun_slot
                   then (* Gun is dead, restore from backup, maybe? *)
                       case !backup
                       of B.Progress =>
                           let in
                               if !was_stuck
                               then eprint ("Gun slot is dead!")
                               else ();
                               was_stuck := true;
                               Can'tRun
                           end
                        | B.Done (cb, ()) =>
                           let
                               val newgun = cb ()
                               val (newbackup, backup_pid) = B.backupspawn dos { src = newgun, use_addressable = false, done_callback = fn () => () }
                           in
                               eprint ("Gun was dead, but restoring from backup.");
                               was_stuck := false;
                               mode := FindTarget { gun_slot = newgun, target_slot = target_slot, backup = newbackup };
                               taketurn dos
                           end
                   else if LTG.slotisdead (GS.myside gs) target_slot
                   then
                       let in
                           if !was_stuck
                           then eprint ("Target slot is dead!")
                           else ();
                           was_stuck := true;
                           Can'tRun
                       end
                   else
                   let
                     val slots = List.tabulate (256, fn i =>
                                                (i, GS.scoreopponentslot gs i))

                     (* Maybe should have a lower bound on what it will
                        consider valuable, and just heal/revive if there
                        are no current high-value targets. *)
                     val (best, _) = ListUtil.max compare_scores slots

                     (* Put the number in our targeting slot. *)
                     val prog = Kompiler.compile (Int (255 - best)) target_slot

                     (* Ignore child pid since we never kill it. *)
                     val (stat, child_pid) = EP.emitspawn dos prog
                   in
                     mode := Attacking { shots = ref 0,
                                         gun_slot = gun_slot,
                                         target_slot = target_slot,
                                         target = best,
                                         status = stat,
                                         backup = backup };
                     was_stuck := false;
                     Can'tRun
                   end
              | Attacking { status = ref (EP.Progress _), ... } =>
                      let in
                          if !was_stuck
                          then eprint ("Unstuck! Thanks!")
                          else ();
                          was_stuck := false;
                          Can'tRun
                      end
              | Attacking { status = ref EP.Done, shots, gun_slot, target, target_slot, backup } =>
                 if LTG.slotisdead (GS.myside gs) gun_slot
                 then 
                     case !backup
                     of B.Progress =>
                         let in
                             if !was_stuck
                             then eprint ("Gun slot is dead!")
                             else ();
                             was_stuck := true;
                             Can'tRun
                         end
                      | B.Done (cb, ()) =>
                         let
                             val newgun = cb ()
                             val (newbackup, backup_pid) = B.backupspawn dos { src = newgun, use_addressable = false, done_callback = fn () => () }
                         in
                             eprint ("Gun was dead, but restoring from backup.");
                             was_stuck := false;
                             mode := Attacking { status = ref EP.Done,
                                                 shots = shots,
                                                 gun_slot = newgun,
                                                 target = target,
                                                 target_slot = target_slot,
                                                 backup = newbackup };
                             taketurn dos
                         end
                 else if LTG.slotisdead (GS.myside gs) target_slot
                 then
                     let in
                          if !was_stuck
                          then eprint ("Sniper's target slot " ^
                                       Int.toString target_slot ^
                                       " was killed! Hoping for medic.\n")
                          else ();
                          was_stuck := true;
                          Can'tRun
                     end
                 else
                     let val theirside = GS.theirside gs
                         val health = Array.sub (#2 theirside, target)
                         val num = Array.sub (#1 (GS.myside gs), target_slot)

(*
                         val () = eprint ("target_slot contains: " ^
                                          LTG.valtos num ^ " and target health is " ^
                                          Int.toString health)

                         val () = Array.appi (fn (i, n) =>
                                              if n < 10000
                                              then eprint ("But " ^ Int.toString i ^ " has vitality " ^
                                                           Int.toString n)
                                              else ()) (#2 theirside)
*)
                     in
                         was_stuck := false;
                         if health <= 0
                         then
                             let in
                                 eprint ("Success! Killed slot " ^
                                         Int.toString target ^
                                         " in " ^ Int.toString (!shots) ^ " shots.");
                                 mode := FindTarget { gun_slot = gun_slot,
                                                      target_slot = target_slot,
                                                      backup = backup };
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

