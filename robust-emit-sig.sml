(* A version of EMIT_PROGRAM that uses Backup. *)
signature ROBUST_EMIT =
sig

  datatype status =
      (* Fraction of program that has been played, in [0, 1) *)
      Progress of real
      (* Can't continue, because the next cell in the
         program (given) is dead, and there is no working backup. *)
      (* TODO: If the cell is killed and zombied, then we probably
         want to indicate that we're just toast. *)
    | Paused of int
      (* Finished emitting. The int is the slot number we used. *)
    | Done of int

  (* Pass in something reasonable for backup_stride. 20 is a good bet. *)
  type emit_args = { turns : LTG.turn list, use_addressable : bool,
                     backup_stride : int }

  (* Create the dominator. Must be spawned by the caller. 
     The child dominator will ignore the slots that the turn list modifies;
     instead, it will assume the turns all operate on the same slot, and
     generate them in a custom slot. (An exception will be thrown if that
     invariant is not held.) *)
  val emit : DOS.dos -> emit_args -> status ref * DOS.dominator

  (* Emit at the process's current priority, returning the pid. *)
  val emitspawn : DOS.dos -> emit_args -> status ref * DOS.pid

end
