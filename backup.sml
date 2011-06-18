structure Backup :> BACKUP =
struct

  type backup = { dest : int, moves : LTG.turn list ref }

  fun moves_to_copy src dest =
    (LTG.LeftApply (Card.Get,dest))::(Kompiler.compile (Kompiler.Int src) dest)

  fun create_backup dos src prio =
    let
      (* reserve a slot in which to take a backup *)
      val slot = (if prio then DOS.reserve_addressable_slot
                  else DOS.reserve_slot) dos
    in
      (case slot of
            (* slot allocation succeeded *)
            SOME dest =>
              SOME ({ dest = dest, moves = ref (moves_to_copy src dest) })
            (* or not *)
          | NONE => NONE)
    end

  fun build_backup ({ dest, moves }) =
    (case !moves of
          m::rest => (moves := rest; SOME m)
        | nil => NONE)

  fun get_backup ({ dest, moves }) =
    if List.null (!moves) then SOME dest else NONE

  fun need_restore dos src =
    LTG.slotisdead (GameState.myside (DOS.gamestate dos)) src 

  fun backup_got_killed dos ({ dest, moves }) = need_restore dos dest

  fun release_backup dos ({ dest, moves }) = DOS.release_slot dos dest

end
