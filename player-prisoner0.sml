structure Player :> PLAYER =
struct
  open Kompiler

  type state = LTG.turn list

  fun makeN 0 = Card Card.Zero
    | makeN n = Apply (Card Card.Succ, makeN (n-1))

  fun maketurnlist () =
    (*
    (compile (Lambda("x", Int 7)) 13) @ [LTG.RightApply (13, LTG.Zero)]
    *)
    let
      val one = Apply (Card Card.Succ, Card Card.Zero)
      val attackpos = makeN 4
      val dec = Apply (Card Card.Dec, attackpos)
      val inc = Apply (Card Card.Inc, Card Card.Zero)
    in
      (compile inc 0) @ (compile dec 1)
    end

  fun init _ = let val l = maketurnlist () in (hd l, tl l) end

  fun round (_, turns) =
    if null turns then init NONE
    else (hd turns, tl turns)

end
