structure OverRipe :> DOMINATOR =
struct
  structure GS = GameState
  datatype src = datatype Kompiler.src

  infix 9 --
  val op -- = Apply
  val $ = Var
  fun \ x exp = Lambda (x, exp)
  infixr 1 `
  fun a ` b = a b

  datatype mode =
      FindTarget
      (* Building the attack program for the given
         cell. *)
    | Emit of { myslot : int, target : int, turns : LTG.turn list }
      (* Keep attacking this slot until it's dead, then find a new target.
       * Note that backing-up is part of this phase. *)
    | Attacking of { myslot : int, target : int, backup : Backup.backup option }

  val compare_scores = ListUtil.bysecond Real.compare


  fun create () =
  let

    (* Maybe should have a lower bound on what it will
       consider valuable, and just heal/revive if there
       are no current high-value targets. *)

    (* Makes a program that attacks the target slot index,
       and then returns itself (so it sticks around). *)
    fun attackprogram target prog_slot =
      let
          (* Numbers are reversed when attacking opponent. *)
          val revtarget = 255 - target

          val dec = 
              (\"f" ` $"f" -- ($"f" -- ($"f" -- ($"f" -- $"f")))) --
              (\"_" ` Card LTG.Dec -- Int revtarget)

          val prog = Kompiler.run_and_return_self dec
      in
          Kompiler.compile prog prog_slot
      end handle (e as Kompiler.Kompiler s) =>
          let in
              eprint ("Kompilation failed: " ^ s ^ "\n");
              raise e
          end

    val mode = ref FindTarget
    fun taketurn dos =
        let val gs = DOS.gamestate dos
        in
            case !mode of
                FindTarget =>
                 (case DOS.reserve_addressable_slot dos of 
                      NONE => DOS.Can'tRun
                    | SOME prog_slot =>
                   let
                     val theirside = GS.theirside gs

                     (* Find the highest-value slot in the
                        opponent's state, according to the
                        stats *)
                     val stats = GS.theirstats gs
                     val slots = List.tabulate (256, fn i =>
                                                (i, GS.scoreopponentslot gs i))

                     (* Maybe should have a lower bound on what it will
                        consider valuable, and just heal/revive if there
                        are no current high-value targets. *)
                     val (best, _) = ListUtil.max compare_scores slots

                     val prog = attackprogram best prog_slot
                   in
                     eprint ("New target: " ^ Int.toString best ^ "\n");
                     mode := Emit { myslot = prog_slot,
                                    target = best, 
                                    turns = prog };
                    taketurn dos
                   end)
              | Emit { myslot, target, turns = nil } => 
                 let 
                   (* Done emitting. Start to create a backup. *)
                   val backup = Backup.create_backup dos myslot false
                 in
                   mode := Attacking { myslot = myslot, target = target,
                                       backup = backup };
                   taketurn dos
                 end
              | Emit { myslot, target, turns = t :: rest } =>
                 let in
                     mode := Emit { myslot = myslot,
                                    target = target,
                                    turns = rest }; 
                     DOS.Turn t
                 end

              | Attacking { myslot, target, backup } =>
                 let val theirside = GS.theirside gs
                     val health = Array.sub (#2 theirside, target)
                     fun do_attack () =
                       let in
                         if health <= 0
                         then 
                             let in
                                 eprint ("Success! Killed slot " ^
                                         Int.toString target ^ "\n");
                                 DOS.release_slot dos myslot;
                                 (case backup of
                                       SOME b => Backup.release_backup dos b
                                     | NONE => ());
                                 mode := FindTarget;
                                 taketurn dos
                             end
                         else 
                             let in
                                 (* Otherwise keep attacking. *)
                                 DOS.Turn (LTG.RightApply (myslot, LTG.I))
                             end
                       end
                   fun do_die () =
                     let in
                       eprint ("overripe got killed in slot " ^
                               Int.toString myslot ^
                               " - starting over\n");
                       DOS.release_slot dos myslot;
                       mode := FindTarget;
                       taketurn dos
                     end
                   fun do_restore newslot =
                     let
                       val new_backup = Backup.create_backup dos newslot false
                     in
                       eprint ("overripe got killed in slot " ^
                               Int.toString myslot ^
                               " - restoring into new slot " ^
                               Int.toString newslot ^ "\n");
                       mode := Attacking { myslot = newslot, target = target,
                                           backup = new_backup };
                       DOS.release_slot dos myslot; (* free the old slot *)
                       taketurn dos
                     end
                 in
                   (case backup of
                         SOME b =>
                           (* CASE 1: was the backup killed? if so, keep
                            * attacking without a backup - TODO: it might be
                            * better to try again, but ... *)
                           if Backup.backup_got_killed dos b then
                             let in
                               mode := Attacking { myslot = myslot,
                                                   target = target,
                                                   backup = NONE };
                               Backup.release_backup dos b;
                               taketurn dos
                             end
                           (* CASE 2: did we get killed? *)
                           else if Backup.need_restore dos myslot then
                             (* can only restore if the backup is built *)
                             (case Backup.get_backup b of
                                   (* we got killed, but it is OK *)
                                   SOME newslot => do_restore newslot
                                 | NONE => (* start over from scratch *)
                                     (Backup.release_backup dos b; do_die ()))
                           (* CASE 3: does the backup still need to be built? *)
                           else
                             (case Backup.build_backup b of
                                   SOME turn => DOS.Turn turn
                                 | NONE => do_attack ())
                       (* backup creation failed; attacking without *)
                       | NONE =>
                           if Backup.need_restore dos myslot then do_die ()
                           else do_attack ())
                 end
        end
  in
    { taketurn = taketurn }
  end
end

