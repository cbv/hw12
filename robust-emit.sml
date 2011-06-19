structure RobustEmit :> ROBUST_EMIT =
struct
  structure GS = GameState

  datatype status =
      Progress of real
    | Paused of int
    | Done of int

  datatype build_state =
      (* no slot allocated; hence no backup thread running *)
      Init of { turnsleft : LTG.turn list }
      (* after a slot is allocated, a backup thread should be spawned *)
    | Backed of { slot : int, turnsleft : LTG.turn list,
                  backup_status : LTG.turn list Backup.status ref }

  fun emit ({ turns, use_addressable }) =
      let 
          val icr = 1.0 / real (length turns + 1)
          val progress = ref 0.0
          val status = ref (Progress 0.0)

          val build_state = ref (Init { turnsleft = turns })

          fun get_turnsleft () =
            let val { turnsleft = turnsleft, ... } in turnsleft end

          fun preview _ = ()

          fun taketurn dos =
            let
              (* To spawn a new backup thread for the given slot *)
              fun spawn_new_backup slot =
                let
                  val (backup_status, _) =
                    Backup.backupspawn dos
                      { src = slot, use_addressable = use_addressable,
                        done_callback = get_turnsleft }
                in
                  backup_status
                end
            in
              (case !build_state of
                    Init { turns } => (* We need to get resources apparently *)
                      let
                        val ret =
                          if use_addressable then DOS.reserve_addressable_slot dos
                          else DOS.reserve_slot dos
                      in
                        (case ret of
                              NONE => DOS.Can'tRun (* Nothing to do ... *)
                            | SOME slot =>
                                let
                                  val backup_status = spawn_new_backup slot
                                in
                                  (* kick the state machine *)
                                  build_state :=
                                    Backed { slot = slot, turnsleft = turns,
                                             backup_status = backup_status };
                                  taketurn dos
                                end)
                      end
                    (* TODO: start new backups periodically...
                     * the state should have another field that's a counter that
                     * counts to 20 or so and then starts backup again *)
                    Backed { slot, turns, backup_status } =>
                      if LTG.slotisdead (GS.myside (DOS.gamestate dos)) slot then
                        case !backup_status of
                             Backup.Progress => DOS.Can'tRun (* nothing to do *)
                           | Backup.Done (get_slot, newturns) =>
                               let (* switching to a backup. *)
                                 val newslot = get_slot ()
                                 val newbackup_status = spawn_new_backup newslot
                               in
                                 build_state :=
                                   Backed { slot = newslot,
                                            turnsleft = newturns,
                                            backup_status = newbackup_status };
                                 DOS.release_slot dos slot
                                 taketurn dos
                               end
                      else
                        (* we are not dead; perform as normal *)
                        (* TODO: increment counter around here or something *)
              )
            end
             (* Old code follows...  

            (case !turnsleft of
                 (* Weird; should only happen for the empty program. *)
                 nil => (status := Done;
                         DOS.kill (DOS.getpid dos);
                         DOS.Can'tRun)
               | (t :: turns) =>
                  let val gs = DOS.gamestate dos
                      val slot = case t of
                          LTG.LeftApply (_, i) => i
                        | LTG.RightApply (i, _) => i
                  in
                      if LTG.slotisdead (GS.myside gs) slot
                      then (status := Paused slot;
                            (* Someone might still revive this slot,
                               but we'll be blocked until then. *)
                            DOS.Can'tRun)
                      else (progress := !progress + icr;
                            (* Immediately be finished if that was the
                               last instruction. *)
                            if List.null turns
                            then status := Done
                            else status := Progress (!progress);
                            turnsleft := turns;
                            DOS.Turn t)
                  end)
                  *)
      in
          (status, { preview = preview, taketurn = taketurn })
      end

  fun emitspawn dos args = 
      let val (status, dom) = emit args
          val pid = DOS.spawn (SOME (DOS.getpid dos))
                              ("EP", DOS.getpriority dos, dom)
      in
          (status, pid)
      end

end
