structure Ripens :> DOMINATOR =
struct
  structure GS = GameState
  datatype src = datatype Kompiler.src
  datatype dosturn = datatype DOS.dosturn

  structure EP = EmitProgram

  infix 9 --
  val op -- = Apply
  val $ = Var
  fun \ x exp = Lambda (x, exp)
  infixr 1 `
  fun a ` b = a b

  datatype mode =
      FindTarget
      (* Once the program is in place, keep attacking until it's dead,
         then find a new target. *)
    | Attacking of { status : EP.status ref, 
                     myslot : int, 
                     target : int }

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

    fun preview dos = ()

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
                     val slots = List.tabulate (256, fn i =>
                                                (i, GS.scoreopponentslot gs i))

                     (* Maybe should have a lower bound on what it will
                        consider valuable, and just heal/revive if there
                        are no current high-value targets. *)
                     val (best, _) = ListUtil.max compare_scores slots

                     val prog = attackprogram best prog_slot

                    (* Ignore child pid since we never kill it. *)
                     val (stat, child_pid) = EP.emitspawn dos prog
                   in
                     eprint ("New target: " ^ Int.toString best ^ "\n");

                     mode := Attacking { myslot = prog_slot,
                                         target = best, 
                                         status = stat };
                     Can'tRun
                   end)
              | Attacking { status = ref (EP.Progress _), ... } => Can'tRun
              | Attacking { status = ref EP.Done, myslot, target } =>
                 let val theirside = GS.theirside gs
                     val health = Array.sub (#2 theirside, target)
                 in
                     if health <= 0
                     then 
                         let in
                             eprint ("Success! Killed slot " ^
                                     Int.toString target ^ "\n");
                             DOS.release_slot dos myslot;
                             mode := FindTarget;
                             taketurn dos
                         end
                     else 
                         let in
                             (* Otherwise keep attacking. *)
                             DOS.Turn (LTG.RightApply (myslot, LTG.I))
                         end
                 end

              (* Optimistically hope that someone will heal it? *)
              | Attacking { status = ref (EP.Paused _), ... } => Can'tRun
        end
  in
    { preview = preview,
      taketurn = taketurn }
  end
end

