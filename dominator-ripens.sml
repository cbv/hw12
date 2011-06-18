structure Ripens :> DOMINATOR =
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
      (* Keep attacking this slot until it's dead,
         then find a new target. *)
    | Attacking of { myslot : int, target : int }

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

                   val prog_slot = DOS.reserve_addressable_slot dos
                   val prog = attackprogram best prog_slot
                 in
                   eprint ("New target: " ^ Int.toString best ^ "\n");
                   mode := Emit { myslot = prog_slot,
                                  target = best, 
                                  turns = prog };
                   taketurn dos
                 end
              | Emit { myslot, target, turns = nil } => 
                 let in
                     mode := Attacking { myslot = myslot, target = target };
                     taketurn dos
                 end
              | Emit { myslot, target, turns = t :: rest } =>
                 let in
                     mode := Emit { myslot = myslot,
                                    target = target,
                                    turns = rest }; 
                     t
                 end

              | Attacking { myslot, target } =>
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
                             LTG.RightApply (myslot, LTG.I)
                         end
                 end
        end
  in
    { taketurn = taketurn }
  end
end

