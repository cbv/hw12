(* This is library that creates a dominator that emits a program (series
   of turns) in the background, with the ability to see its status. 

   This makes sure that we don't waste time by trying to write into
   dead slots. In the future, it may be able to pull tricks to generate
   the program more robustly.
*)
signature EMIT_PROGRAM =
sig

  datatype status =
      (* Fraction of program that has been played, in [0, 1) *)
      Progress of real
      (* Can't continue, because the next cell in the
         program (given) is dead. *)
    | Paused of int
      (* Finished emitting. *)
    | Done

  (* Create the dominator. Must be spawned by the caller. 
     You should own all of the slots that this turn list
     modifies, since the child dominator will access them. *)
  val emit : LTG.turn list -> status ref * DOS.dominator

  (* Emit at the process's current priority, returning the pid. *)
  val emitspawn : DOS.dos -> LTG.turn list -> status ref * DOS.pid

end
