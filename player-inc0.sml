
structure Inc0 :> LAYER =
struct
  structure GS = GameState

  (* This one doesn't pay any attention to the
     game state. *)
  fun init gs = ()


  val n = ref 0
  val parity = ref false
  fun taketurn gs =
    let
      fun makezero () = LTG.RightApply (0, LTG.Zero)
      fun heal () = LTG.LeftApply (LTG.Inc, 0)
      val turn =
          if !parity
          then heal ()
          else makezero ()
    in
      (* Sorry, should be leaner now... *)
      n := !n + 1;
(*
      if !n mod 1000 = 0
      then (eprint ("Turn #" ^ Int.toString (!n) ^ ". Stats:\n");
            GS.printstats (GS.mystats gs);
            eprint "Theirs:\n";
            GS.printstats (GS.theirstats gs))
      else ();
*)
      parity := not (!parity);
      turn
    end

end

structure Player = LayerFn(Inc0)
