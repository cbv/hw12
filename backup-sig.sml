(* Provides a way for Dominators to save copies of their work.
 * Currently does not support partial backups. Each backup tracks only one slot,
 * and the slot should not be depended upon by anything (i.e. don't ask me to
 * backup slot 0 if you have something that uses what you have in slot 0 and has
 * that slot number baked into it -- unless you intend to restore from the
 * backed-up-into slot into the original slot (get_backup lets you do it however
 * you want.)). *)
(* so the other day I was thinking um wait no what was I talking about did 
 * you hear that? was that a bird? if so that was a really weird bird *)
signature BACKUP =
sig

  (* backup internal state *)
  type backup

  (* given gamestate, a slot to take a backup of, and whether to optimise to
   * make restoration high-prio (i.e. easily addressable), returns a backup
   * state and the moves it will take to generate the backup *)
  val create_backup : DOS.dos -> int -> bool -> backup option

  val build_backup : backup -> LTG.turn option

  (* returns the slot number the backup is in, IF the backup is valid *)
  val get_backup : backup -> int option

  (* returns the slot number for the backup (useful to tell if the backup itself
   * has died) *)
  val get_backup_slot : backup -> int

  (* did the mans get killed (pass in the slot number) *)
  val need_restore : DOS.dos -> int -> bool

  (* need to release resources -- if you switch to using the backup slot as your
   * new slot, do not call this, but if you're going away entirely or otherwise
   * stop needing the backup, call this *)
  val release_backup : DOS.dos -> backup -> unit

end
