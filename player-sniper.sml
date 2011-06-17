
structure Sniper :> LAYER =
struct
  structure GS = GameState
  datatype src = datatype Kompiler.src

  (* This one doesn't pay any attention to the
     game state. *)
  fun init gs = ()

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

  fun scoreslot (idx : int, s : LTG.stat) =
      (idx,
       (* XXX weighted! *)
       (* XXX probably should be low priority if already dead :) *)
       real (LTG.stat_left_applications s) +
       real (LTG.stat_right_applications s) +
       LTG.stat_damage_done s +
       LTG.stat_healing_done s +
       real (LTG.stat_iterations s) +
       real (LTG.stat_gotten s))

  val compare_scores = ListUtil.bysecond Real.compare

  (* Takes an expression of object type unit.
     Wraps in a function that ignores its single argument,
     evaluates the expression, and then returns itself. 

     fun f _ = (exp; f)
     *)
  fun returnself src =
      let val recursive =
          Lambda ("self",
                  Lambda ("x",
                          Apply (Apply (Card LTG.Put,
                                        src),
                                 Var "self")))

          val minifix =
              Lambda ("x",
                      Apply (Var "f", Apply (Var "x", Var "x")))
          val fix =
              Lambda ("f",
                      Apply (minifix, minifix))
      in
          Apply (fix, recursive)
      end

  fun attackprogram target =
    let
        (* XXX should dec as much as we can in this turn,
           without exceeding 1000. *)
        val dec = Apply (Card LTG.Dec, Int target)
        val prog = returnself dec
        (* val prog = dec *)
    in
        Kompiler.compile prog ATTACK_SLOT
    end


  val mode = ref FindTarget
  fun taketurn gs =
      case !mode of
          FindTarget =>
           let
             (* Find the highest-value slot in the
                opponent's state, according to the
                stats *)
             val stats = GS.theirstats gs
             val slots = List.tabulate (256, fn i =>
                                        (i, LTG.statfor stats i))
             val slots = map scoreslot slots

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
           in
               if Array.sub (#2 theirside, i) <= 0
               then (eprint ("Success! Killed slot " ^
                             Int.toString i);
                     mode := FindTarget;
                     taketurn gs)
                   (* Otherwise keep attacking. *)
               else LTG.RightApply (ATTACK_SLOT, LTG.I)
           end

end

structure Player = LayerFn(Sniper)
