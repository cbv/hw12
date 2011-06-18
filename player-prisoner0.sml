structure Player :> PLAYER =
struct
  open Kompiler

  (* keeps track of the current turn and a list of next moves. *)
  type state = int * (LTG.turn list)

  (* obsoleted by kompiler Int
  fun makeN 0 = Card Card.Zero
    | makeN n = Apply (Card Card.Succ, makeN (n-1))
  *)

  fun load_num n i = compile (Int n) i

  fun maketurnlist k =
    let
      fun apply instr arg = (compile instr 13) @ [LTG.RightApply (13, arg)]
      val decpos = Int (if k < 30000 then 0 else 1)
      val incpos = Int 0
      val respos = Int 255
      val dec = Apply (Card Card.Dec, decpos)
      val inc = Apply (Card Card.Inc, incpos)
      val res = Apply (Card Card.Revive, respos)
      (*val cond_res = (* check if dead *)*)
    in
      (compile inc 0) @ (compile dec 1) @ (compile dec 1)
    end

  fun init _ = let val l = maketurnlist 0 in (hd l, (1, tl l)) end

  fun round (_, (k, turns)) =
    if null turns then let val l = (maketurnlist k) in (hd l, (k+1, tl l)) end
    else (hd turns, (k+1, tl turns))

end
