(* Note, I started using "sniper" again.
   This should be better than "ripens". -tom7 *)
structure Sniper :> DOMINATOR =
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
                     shots : int ref,
                     myslot : int, 
                     target : int }

  val compare_scores = ListUtil.bysecond Real.compare

  val eprint = fn s => eprint ("[SNIPER] " ^ s ^ "\n")

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

          fun repeat 1 e = e
            | repeat n e = repeat (n - 1) e -- e

          val dec = 
              (\"target" `
               (* empirical. Attack is MUCH more efficient.
                  Should use attack. *)
               repeat 60 (Card LTG.Dec -- $"target")) --
              Int revtarget

          val prog = Kompiler.run_and_return_self dec
      in
          Kompiler.compile prog prog_slot
      end handle (e as Kompiler.Kompiler s) =>
          let in
              eprint ("Kompilation failed: " ^ s ^ "\n");
              raise e
          end

    fun preview dos = ()

    (* This is just for diagnostics. *)
    val was_stuck = ref false

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
                     eprint ("New target: " ^ Int.toString best ^
                             ". Program length: " ^ Int.toString (length prog));

                     mode := Attacking { myslot = prog_slot,
                                         shots = ref 0,
                                         target = best, 
                                         status = stat };
                     was_stuck := false;
                     Can'tRun
                   end)
              | Attacking { status = ref (EP.Progress _), ... } => 
                      let in
                          if !was_stuck
                          then eprint ("Unstuck! Thanks!")
                          else ();
                          was_stuck := false;
                          Can'tRun
                      end
              | Attacking { status = ref EP.Done, shots, myslot, target } =>
                 let val theirside = GS.theirside gs
                     val health = Array.sub (#2 theirside, target)
                 in
                     if health <= 0
                     then 
                         let in
                             eprint ("Success! Killed slot " ^
                                     Int.toString target ^
                                     " in " ^ Int.toString (!shots) ^ " shots.");
                             DOS.release_slot dos myslot;
                             mode := FindTarget;
                             taketurn dos
                         end
                     else 
                         let in
                             (* Otherwise keep attacking. *)
                             shots := !shots + 1;
                             DOS.Turn (LTG.RightApply (myslot, LTG.I))
                         end
                 end

              (* Optimistically hope that medic will heal it? *)
              | Attacking { status = ref (EP.Paused i), ... } => 
                 let in
                     if !was_stuck
                     then ()
                     else eprint ("Stuck because slot " ^ Int.toString i ^ " is dead.");
                     was_stuck := true;
                     Can'tRun
                 end
        end
  in
    { preview = preview,
      taketurn = taketurn }
  end
end

