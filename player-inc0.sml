
structure Inc0 :> LAYER =
struct
  structure GS = GameState

  (* This one doesn't pay any attention to the
     game state. *)
  fun init gs = ()


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
      GS.printstats (GS.mystats gs);
      parity := not (!parity);
      turn
    end

end

structure Player = LayerFn(Inc0)
