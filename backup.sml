structure Backup :> BACKUP =
struct

  exception Fuck of string

  datatype 'a status = Progress | Done of ((unit -> int) * 'a)

  (* TODO: replace this with calling the NumberGenerator *)
  fun moves_to_copy src dest =
    (Kompiler.compile (Kompiler.Int src) dest)@[LTG.LeftApply (Card.Get,dest)]

  fun slotisdead dos src =
    LTG.slotisdead (GameState.myside (DOS.gamestate dos)) src 

  (* the passed in dos is the parent's pid, so allocating slots on that dos
   * results in us not having to transfer ownership back to the parent.*)
  fun backup dos_parent src prio callback =
    let
      (* 'dos' is guaranteed (by spoons) to be just global state and a pid. *)
      fun get_cell_and_moves () =
        let val ret = if prio then DOS.reserve_addressable_slot dos_parent
                               else DOS.reserve_slot dos_parent
        in (case ret of
                 NONE => NONE
               | SOME dest => SOME (dest, moves_to_copy src dest))
        end

      (* IPC with the parent process *)
      val status = ref Progress
      (* This holds the dest cell that we are backing up into and also the moves
       * remaining to finish the backup. *)
      val dest_and_moves = ref (get_cell_and_moves ())

      (* Called by the parent when it adopts the backup *)
      fun finished dos () =
        (case !dest_and_moves of
              SOME (dest, _) => (DOS.kill (DOS.getpid dos); dest)
            | NONE => raise Fuck "backup invariant 1 violated")

      (* Preview takes care of a few pre-checks:
       * 1 - Do we have an allocated slot for the destination?
       * 2 - Did our destination slot get killed? 
       * Other things, like is the source slot alive, and are we done making the
       * backup, are handled in taketurn. *)
      fun preview dos =
        let in
          (* If OOMed before, attempt to reallocate. may fail anyway. *)
          if (!dest_and_moves) = NONE then
            (* TODO: Allocating during preview is more rude than in taketurn. *)
            dest_and_moves := get_cell_and_moves ()
          else ();
          (* Did the backup get killed *)
          (case !dest_and_moves of
                NONE => ()
              | SOME (dest, _) =>
                  if slotisdead dos dest then (* we need to relocate *)
                    let in
                      (* The slot is owned in the parent's name. *)
                      DOS.release_slot dos_parent dest;
                      status := Progress;
                      dest_and_moves := NONE;
                      (* will attempt to reallocate, but can never loop because
                       * the allocator does not return dead slots *)
                      preview dos
                    end
                  else ())
        end

      (* Main.
       * If we are done backing up or if the source is killed, we sit.
       * Otherwise continues to make a backup. *)
      fun taketurn dos = 
        let
          (* Invariant check: "Done" and the move list being [] are linked. *)
          fun check_done_status () =
            case !status of
                 Progress => raise Fuck "backup invariant 2 violated" | _ => ()
          (* Takes a step. *)
          fun do_backup_move dest m rest =
            (dest_and_moves := SOME (dest, rest); DOS.Turn m)
        in
          (case !dest_and_moves of
                NONE => DOS.Can'tRun
              (* Indicates that we are done building. *)
              | SOME (dest, []) => (check_done_status (); DOS.Can'tRun)
              (* The final move, which will complete the backup. *)
              | SOME (dest, m::[]) =>
                  if slotisdead dos src then DOS.Can'tRun
                  else
                    let in
                      status := Done (finished dos, callback ());
                      do_backup_move dest m []
                    end
              (* Still in progress. *)
              | SOME (dest, m::rest) =>
                  if slotisdead dos src then DOS.Can'tRun
                  else do_backup_move dest m rest
          )
        end
    in
      (status, { preview = preview, taketurn = taketurn })
    end

  fun backupspawn dos_parent src prio callback =
    let val (status, dom) = backup dos_parent src prio callback
        val pid = DOS.spawn (SOME (DOS.getpid dos_parent))
                            (DOS.getpriority dos_parent, dom)
    in (status, pid)
    end

end
