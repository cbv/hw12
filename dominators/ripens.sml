structure Ripens :> DOMINATOR =
struct
  structure GS = GameState
  datatype src = datatype Kompiler.src

  fun create () =
  let

    (* Hard to kill. *)
    val ATTACK_SLOT = 0

    datatype mode =
        FindTarget
        (* Building the attack program for the given
           cell. *)
      | Emit of int * LTG.turn list
        (* Keep attacking this slot until it's dead,
           then find a new target. *)
      | Attacking of int

    val compare_scores = ListUtil.bysecond Real.compare

    (* Maybe should have a lower bound on what it will
       consider valuable, and just heal/revive if there
       are no current high-value targets. *)

    infix 9 --
    val op -- = Apply
    val $ = Var
    fun \ x exp = Lambda (x, exp)
    infixr 1 `
    fun a ` b = a b

    (* Makes a program that attacks the target slot index,
       and then returns itself (so it sticks around). *)
    fun attackprogram target =
      let
          (* Numbers are reversed when attacking opponent. *)
          val revtarget = 255 - target

          val dec = 
              (\"f" ` $"f" -- ($"f" -- ($"f" -- ($"f" -- $"f")))) --
              (\"_" ` Card LTG.Dec -- Int revtarget)

          val prog = Kompiler.run_and_return_self dec
      in
          Kompiler.compile prog ATTACK_SLOT
      end handle (e as Kompiler.Kompiler s) =>
          let in
              eprint ("Kompilation failed: " ^ s ^ "\n");
              raise e
          end

    val n = ref 0
    val mode = ref FindTarget
    fun taketurn dos =
        let val gs = DOS.gamestate dos
        in
            n := !n + 1;
            if !n mod 1000 = 0
            then (eprint ("Turn #" ^ Int.toString (!n) ^ ". Stats:\n");
                  GS.printstats (GS.mystats gs);
                  eprint "Theirs:\n";
                  GS.printstats (GS.theirstats gs))
            else ();

            LTG.enable_trace false;

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

                   val prog = attackprogram best
                   (* val () = eprint ("Program: " ^ LTG.turns2str prog ^ "\n") *)

                 in
                   eprint ("New target: " ^ Int.toString best ^ "\n");
                   mode := Emit (best, prog);
                   taketurn gs
                 end
              | Emit (i, nil) => (mode := Attacking i; taketurn gs)
              | Emit (i, (t :: rest)) => (mode := Emit (i, rest); t)

              | Attacking i =>
                 let val theirside = GS.theirside gs
                     val health = Array.sub (#2 theirside, i)
                 in
                     if health <= 0
                     then (eprint ("Success! Killed slot " ^
                                   Int.toString i ^ "\n");
                           mode := FindTarget;
                           taketurn gs)
                         (* Otherwise keep attacking. *)
                     else 
                         let in
  (*
                             eprint ("Target " ^ Int.toString i ^ 
                                     "'s health: " ^ Int.toString health ^ "\n");
  *)
                             LTG.RightApply (ATTACK_SLOT, LTG.I)
                         end
                 end
        end
  in
    { taketurn = taketurn }
  end
end

