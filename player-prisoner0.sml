
structure Prisoner0 :> LAYER =
struct
  structure GS = GameState

  (* This one doesn't pay any attention to the
     game state. *)
  fun init gs = ()

  structure K = Kompiler

  fun makeN 0 = K.Card Card.Zero
    | makeN n = K.Apply (K.Card Card.Inc, (makeN (n-1)))

  fun maketurnlist () =
    let
      (*
      fun makezero () = LTG.RightApply (0, LTG.Zero)
      fun poke () = LTG.LeftApply (LTG.Dec, 255)
      fun heal () = LTG.LeftApply (LTG.Inc, 0)
      *)
      val one = K.Apply (K.Card Card.Inc, K.Card Card.Zero)
      val two55 = makeN 255
      val stab = K.Apply (K.Card Card.Dec, two55)
      val heal = K.Apply (K.Card Card.Inc, K.Card Card.Zero)
    in
      (K.compile heal 0) @ (K.compile stab 1)
    end

  val turnlist = ref []

  fun taketurn gs =
  let val (turn, turns) =
    case (!turnlist) of
         [] => let val l = maketurnlist() in
                (case l of [] => (LTG.LeftApply (LTG.I, 0), [])
                   | (t::ts) => (t, ts))
               end
       | (turn::turns) => (turn, turns)
  in
    (turnlist := turns; turn)
  end

end

structure Player = LayerFn(Prisoner0)
