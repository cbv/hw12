structure RobustEmit :> ROBUST_EMIT =
struct
  exception Fuck of string

  structure GS = GameState

  datatype status =
      Progress of real
    | Paused of int
    | Done of int

  type emit_args = { turns : LTG.turn list, use_addressable : bool }

  datatype build_state =
      (* no slot allocated; hence no backup thread running *)
      Init of { turnsleft : LTG.turn list }
      (* after a slot is allocated, a backup thread should be spawned *)
    | Backed of { slot : int, turnsleft : LTG.turn list,
                  backup_status : (LTG.turn list * real) Backup.status ref }

  fun emit ({ turns, use_addressable }) =
      let 
          val icr = 1.0 / real (length turns + 1)
          val progress = ref 0.0
          val status = ref (Progress 0.0)

          val build_state = ref (Init { turnsleft = turns })

          fun get_turnsleft () =
            (case !build_state of
                  Backed { turnsleft, ... } => (turnsleft, !progress)
                | _ => raise (Fuck "backup callback called in wrong state"))

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
                    Init { turnsleft } => (* We need to get resources apparently *)
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
                                    Backed { slot = slot, turnsleft = turnsleft,
                                             backup_status = backup_status };
                                  taketurn dos
                                end)
                      end
                    (* TODO: start new backups periodically...
                     * the state should have another field that's a counter that
                     * counts to 20 or so and then starts backup again *)
                  | Backed { slot, turnsleft, backup_status } =>
                      (* Check if we are got got? *)
                      if LTG.slotisdead (GS.myside (DOS.gamestate dos)) slot then
                        case !backup_status of
                             Backup.Progress => DOS.Can'tRun (* nothing to do *)
                           | Backup.Done (get_slot, (newturns,newprogress)) =>
                               let (* switching to a backup. *)
                                 val newslot = get_slot ()
                                 val newbackup_status = spawn_new_backup newslot
                               in
                                 progress := newprogress;
                                 build_state :=
                                   Backed { slot = newslot,
                                            turnsleft = newturns,
                                            backup_status = newbackup_status };
                                 DOS.release_slot dos slot;
                                 taketurn dos
                               end
                      (* we are not dead; perform as normal *)
                      else
                        (* TODO: increment counter around here or something *)
                        (case turnsleft of
                              nil => (status := Done slot;
                                      DOS.kill (DOS.getpid dos); DOS.Can'tRun)
                            | (t :: rest) =>
                                let
                                  fun maketurn (LTG.LeftApply (c, _)) =
                                      DOS.Turn (LTG.LeftApply (c, slot))
                                    | maketurn (LTG.RightApply (_, c)) =
                                      DOS.Turn (LTG.RightApply (slot, c))
                                in
                                  progress := !progress + icr;
                                  (* update status field *)
                                  if List.null rest then status := Done slot
                                  else status := Progress (!progress);
                                  build_state :=
                                    Backed { slot = slot, turnsleft = rest,
                                             backup_status = backup_status };
                                  maketurn t
                                end)
              )
            end
      in
          (status, { preview = preview, taketurn = taketurn })
      end

  fun emitspawn dos (args as { turns, use_addressable }) = 
      let
        val (status, dom) = emit args
        val pid = DOS.spawn (SOME (DOS.getpid dos))
                            ("EP", DOS.getpriority dos, dom)
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
