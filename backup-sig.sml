(* Provides a way for Dominators to save copies of their work.
 * Currently does not support partial backups. Each backup tracks only one slot,
 * and the slot should not be depended upon by anything (i.e. don't ask me to
 * backup slot 0 if you have something that uses what you have in slot 0 and has
 * that slot number baked into it -- unless you intend to restore from the
 * backed-up-into slot into the original slot, but that is your job. *)
(* so the other day I was thinking um wait no what was I talking about did 
 * you hear that? was that a bird? if so that was a really weird bird *)
signature BACKUP =
sig

  (* unit -> int is a function to call to let the backup thread know that you
   * are about to adopt the backed-up slot as its own. when the callback is
   * called, you now own that cell, and the backup thread goes away. *)
  datatype 'a status =
      Progress
    | Done of ((unit -> int) * 'a)

  (* Generates a backup thread. The thunk is a callback to store data in the
   * done status flag upon the time of completion of the backup, such as for the
   * EmitProgram partial backup to know at what point to resume building when
   * restoring from a backup. *)
  val backup : DOS.dos -> int -> bool -> (unit -> 'a)
               -> 'a status ref * DOS.dominator
  (* Generates and spawns a backup thread. *)
  val backupspawn : DOS.dos -> int -> bool -> (unit -> 'a)
                    -> 'a status ref * DOS.pid

end
