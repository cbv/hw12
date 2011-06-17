
structure Ripens :> LAYER =
struct
  structure GS = GameState
  datatype src = datatype Kompiler.src

  (* This one doesn't pay any attention to the
     game state. *)
  fun init gs = ()

  fun eprint s = TextIO.output (TextIO.stdErr, s)

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

  (* Maybe should have a lower bound on what it will
     consider valuable, and just heal/revive if there
     are no current high-value targets. *)
  fun scoreslot side (idx : int, s : LTG.stat) =
      (idx,
       (* XXX weighted! *)
       if LTG.slotisdead side idx
       then ~1000.0
       else real (LTG.stat_left_applications s) +
            real (LTG.stat_right_applications s) +
            LTG.stat_damage_done s +
            LTG.stat_healing_done s +
            real (LTG.stat_iterations s) +
            real (LTG.stat_gotten s))

  val compare_scores = ListUtil.bysecond Real.compare


  infix 9 --
  val op -- = Apply
  val $ = Var
  fun \ x exp = Lambda (x, exp)
  infixr 1 `
  fun a ` b = a b

  (* Takes an expression of object type unit.
     Wraps in a function that ignores its single argument,
     evaluates the expression, and then returns itself. 

     fun f _ = (exp; f)
     *)
  fun returnself src =
      let val recursive =
          \"self" ` 
          \"unused" `
          Card LTG.Put -- src -- $"self"

          (* CBV fix-point operator. *)
          val minifix =
              \"x" ` $"f" -- (\"y" ` $"x" -- $"x" -- $"y")
          val fix = \"f" ` minifix -- minifix
      in
          Apply (fix, recursive)
      end

  (* Makes a program that attacks the target slot index,
     and then returns itself (so it sticks around). *)
  fun attackprogram target =
    let
        (* XXX should dec as much as we can in this turn,
           without exceeding 1000. *)

        (* Numbers are reversed when attacking opponent. *)
        val revtarget = 255 - target

        val dec = 
            (\"f" ` $"f" -- ($"f" -- ($"f" -- ($"f" -- $"f")))) --
            (\"_" ` Card LTG.Dec -- Int revtarget)
        val prog = returnself dec
    in
        Kompiler.compile prog ATTACK_SLOT
    end handle (e as Kompiler.Kompiler s) =>
        let in
            eprint ("Kompilation failed: " ^ s ^ "\n");
            raise e
        end

  val n = ref 0
  val mode = ref FindTarget
  fun taketurn gs =
      let in
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
                                            (i, LTG.statfor stats i))
                 val slots = map (scoreslot theirside) slots

                 val (best, _) = ListUtil.max compare_scores slots

                 val prog = attackprogram best
                 val () = eprint ("Program: " ^ LTG.turns2str prog ^ "\n")

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
end

structure Player = LayerFn(Ripens)
