structure Player :> PLAYER =
struct
  open Kompiler

  type state = int * (LTG.turn list)

  fun makeN 0 = Card Card.Zero
    | makeN n = Apply (Card Card.Succ, makeN (n-1))

  fun maketurnlist k =
    let
      fun apply instr arg = (compile instr 13) @ [LTG.RightApply (13, arg)]
      val attackpos = makeN 0
      val dec = Apply (Card Card.Dec, attackpos)
      val inc = Apply (Card Card.Inc, Int 0)
    in
      (compile inc 0) @ (compile dec 1)
    end

  fun init _ = let val l = maketurnlist 0 in (hd l, (1, tl l)) end

  fun round (_, (k, turns)) =
    if null turns then let val l = (maketurnlist k) in (hd l, (k+1, tl l)) end
    else (hd turns, (k+1, tl turns))

end
