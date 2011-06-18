(* Provides a way for Dominators to save copies of their work.
 * Currently does not support partial backups. Each backup *)
signature BACKUP =
sig

  (* backup internal state *)
  type backup

  (* given gamestate, a slot to take a backup of, and whether to optimise to
   * make restoration high-prio (i.e. easily addressable), returns a backup
   * state and the moves it will take to generate the backup *)
  val create_backup : DOS.dos -> int -> bool
                      -> (backup * (unit -> LTG.turn option)) option

  (* returns the slot number the backup is in, IF the backup is valid *)
  val get_backup : backup -> int option

  (* did the mans get killed *)
  val need_restore : DOS.dos -> backup -> bool

end
