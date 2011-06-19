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
      Waiting (* can be used to tell us "don't do anything yet" *)
    | Progress
    | Done of ((unit -> int) * 'a)

  (* src is which slot number to take a backup from;
   * use_addressable is whether to try to use an easily-addressable slot for the
   * backup or not ("high priority location");
   * The done_callback can be used to return user-specific state taken from the
   * exact time the backup was finished; its return value will be stored in the
   * Done status flag ref. this is useful for such as doing partial backups
   * inside of EmitProgram; the "'a" could be the remaining moves needed to
   * finish building at the time the backup was completed. *)
  type 'a backup_args =
    { src : int, use_addressable : bool, done_callback : unit -> 'a }

  (* Generates a backup thread. *)
  val backup : DOS.dos -> 'a backup_args -> 'a status ref * DOS.dominator
  (* Generates and spawns a backup thread. *)
  val backupspawn : DOS.dos -> 'a backup_args -> 'a status ref * DOS.pid

end
