structure RobustEmit :> ROBUST_EMIT =
struct
  exception Fuck of string

  structure GS = GameState

  datatype status =
      Progress of real
    | Paused of int
    | Done of int

  type emit_args = { turns : LTG.turn list, use_addressable : bool,
                     backup_stride : int }

  (* We will use two backups and flip between the two, so we always have a
   * backup on hand even while refreshing to a latest backup. (Note: Killing
   * the backups can still foil us, but we won't get foiled if they only
   * target the main slot.) *)
  type backup_status = ((LTG.turn list * int) Backup.status ref * DOS.pid)

  (* Trigger indicates the next time a backup should be kicked. *)
  type backups = { b0 : backup_status, b1 : backup_status, b_trigger : int }

  datatype build_state =
      (* no slot allocated; hence no backup thread running *)
      Init of { turnsleft : LTG.turn list }
      (* after a slot is allocated, a backup thread should be spawned *)
    | Backed of { slot : int, turnsleft : LTG.turn list, tick_count : int,
                  backups : backups }

  fun emit dos_parent ({ turns, use_addressable, backup_stride }) =
      let 
          val icr = 1.0 / real (length turns + 1)
          val progress = ref 0.0
          val status = ref (Progress 0.0)

          val build_state = ref (Init { turnsleft = turns })

          fun get_turnsleft () =
            (case !build_state of
                  Backed { turnsleft, tick_count, ... } =>
                    (turnsleft, tick_count)
                | _ => raise (Fuck "backup callback called in wrong state"))

          fun preview _ = ()

          fun taketurn dos =
            let
              (* To spawn a new backup thread for the given slot *)
              fun spawn_new_backup slot =
                Backup.backupspawn dos
                  { src = slot, use_addressable = use_addressable,
                    done_callback = get_turnsleft }
              fun two_new_backups ticks slot =
                let
                  val b0 as (b0p,_) = spawn_new_backup slot
                  val b1 as (b1p,_) = spawn_new_backup slot
                in
                  (* b0 should always be the "more recent" backup. Right now,
                   * that means "farther in the future than b1". *)
                  b0p := Backup.Waiting; b1p := Backup.Waiting;
                  { b0 = b0, b1 = b1, b_trigger = ticks + backup_stride }
                end
            in
              (case !build_state of
                    Init { turnsleft } => (* We need to get resources apparently *)
                      let
                        val ret =
                          if use_addressable then DOS.reserve_addressable_slot dos
                          else DOS.reserve_slot dos
                      in
                        (case ret of
                              NONE => DOS.Can'tRun (* Nothing to do ... *)
                            | SOME slot =>
                                let in
                                  (* kick the state machine *)
                                  build_state :=
                                    Backed { slot = slot, turnsleft = turnsleft,
                                             tick_count = 0,
                                             backups = two_new_backups 0 slot };
                                  taketurn dos
                                end)
                      end
                  (* Usual case. *)
                  | Backed { slot, turnsleft, tick_count,
                             backups as { b0, b1, b_trigger } } =>
                      (* Check if we are got got? *)
                      if LTG.slotisdead (GS.myside (DOS.gamestate dos)) slot then
                        let
                          (* A function to transfer the build process to the
                           * backup slot and begin "afresh" *)
                          fun use_backup (get_slot,(newturns,newticks))
                                         other_backup =
                            let
                              val newslot = get_slot () (* Kill the used guy *)
                              val newbackups = two_new_backups newticks newslot
                            in
                              eprint ("RobustEmit killed in slot " ^
                                      Int.toString slot ^ " at " ^
                                      Int.toString tick_count ^
                                      "steps; using backup in " ^
                                      Int.toString newslot ^ " from " ^
                                      Int.toString newticks ^ " steps\n");
                              progress := (Real.fromInt newticks) * icr;
                              build_state :=
                                Backed { slot = newslot, turnsleft = newturns,
                                         tick_count = newticks,
                                         backups = newbackups };
                              (* Free our old slot, which is dead. *)
                              DOS.release_slot dos slot;
                              (* Kill the unused guy *)
                              DOS.kill (#2 other_backup);
                              taketurn dos
                            end
                        in
                          (* b0 is "more recent", so check it first *)
                          case !(#1 b0) of
                               Backup.Done done_info => use_backup done_info b1
                             | _ =>
                                 (case !(#1 b1) of
                                       Backup.Done done_info => (* use older *)
                                         use_backup done_info b0
                                     | _ => (* fucked *)
                                       (status := Paused slot; DOS.Can'tRun))
                        end
                      (* we are not dead; perform as normal *)
                      else
                        let
                          fun kick_backups { b0, b1, b_trigger } =
                            if tick_count + 1 = b_trigger then
                              let
                                val b0new = spawn_new_backup slot
                              in
                                (* eprint ("RobustEmit kicking backups at tick " ^
                                       Int.toString (tick_count + 1) ^ "\n"); *)
                                DOS.kill (#2 b1);
                                { b0 = b0new, b1 = b0,
                                  b_trigger = b_trigger + backup_stride }
                              end
                            else
                              { b0 = b0, b1 = b1, b_trigger = b_trigger }
                        in
                          (case turnsleft of
                                nil => (status := Done slot;
                                        DOS.transfer_slot dos
                                          { dst = dos_parent, slot = slot };
                                        DOS.kill (DOS.getpid dos); DOS.Can'tRun)
                              | (t :: rest) =>
                                  let
                                    fun maketurn (LTG.LeftApply (c, _)) =
                                        DOS.Turn (LTG.LeftApply (c, slot))
                                      | maketurn (LTG.RightApply (_, c)) =
                                        DOS.Turn (LTG.RightApply (slot, c))
                                  in
                                    progress := !progress + icr;
                                    (* update state - has to happen before the
                                     * possible killing self. *)
                                    build_state :=
                                      Backed { slot = slot, turnsleft = rest,
                                               tick_count = tick_count + 1,
                                               backups = kick_backups backups };
                                    (* update status field *)
                                    if List.null rest then
                                      (status := Done slot;
                                       DOS.transfer_slot dos
                                         { dst = dos_parent, slot = slot };
                                       DOS.kill (DOS.getpid dos))
                                    else status := Progress (!progress);
                                    maketurn t
                                  end)
                        end
              )
            end
      in
          (status, { preview = preview, taketurn = taketurn })
      end

  fun emitspawn dos_parent (args as { turns, use_addressable, backup_stride }) = 
      let
        val (status, dom) = emit dos_parent args
        val pid = DOS.spawn (SOME (DOS.getpid dos_parent))
                            ("RE", DOS.getpriority dos_parent, dom)
        (* invariant checking - all slots should be the same *)
        fun checkturn slot (LTG.LeftApply (_, i)) =
            if not (slot = i) then raise (Fuck "bad turns list") else ()
          | checkturn slot (LTG.RightApply (i, _)) =
            if not (slot = i) then raise (Fuck "bad turns list") else ()
        fun checkturns [] = []
          | checkturns (turns as (LTG.LeftApply(_, i)::_)) =
            map (checkturn i) turns
          | checkturns (turns as (LTG.RightApply(i, _)::_)) =
            map (checkturn i) turns
        val _ = checkturns turns (* perform the check *)
      in
          (status, pid)
      end

end
