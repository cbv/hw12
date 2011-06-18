structure Player :> PLAYER =
struct
structure K = Kompiler
datatype src = datatype K.src

type state = LTG.turn list

(*
  At the end both players should be in the state:

    13={10000,7}
    14={10000,help}
    15={10000,help}
    252={9991,I}
    255={9999,I}

  Where the 9991 might be smaller depending on how efficient the compiler is.
  Both player should exhaust the stack while running the program in slot 16.
  Look for this in the judges simulator:

    player 0 applied slot 16 to card zero
    Exception: Native.AppLimitExceeded
    slot 16 reset to I
    
*)

fun init _ = 
    let 
      val p =
          (* Put the program "fn x => 7" into slot 13 and then call it. *)
          (K.compile (Lambda ("x", Int 7)) 13) 
          @ [LTG.RightApply (13, LTG.Zero)]

          (* (\x.\f.f Help x) Copy (\x.\y.x)  ==>  Help *)
          @ (K.compile (Apply
                         (Apply
                           (Lambda ("x", 
                             (Lambda ("f", 
                               Apply (Apply (Var "f", Card Card.Help),
                                      Var "x")))), 
                            Card Card.Copy),
                          Lambda ("x", Lambda ("y", Var "x")))) 14)

          (* (\x.\f.f Help x) (Dec Zero) (\x.\y.x)  ==>  Help *)
          @ (K.compile (Apply
                         (Apply
                           (Lambda ("x", 
                             (Lambda ("f", 
                               Apply (Apply (Var "f", Card Card.Help),
                                      Var "x")))), 
                            (Apply (Card Card.Dec, Card Card.Zero))),
                          Lambda ("x", Lambda ("y", Var "x")))) 15)

          (* Also, run Dec forever to check that we can loop. *)
          @ (K.compile (K.fix (
              Lambda ("self", 
                      Lambda ("_", 
                              Apply (Apply (Card LTG.K, (* doesn't matter *)
                                            (Apply (Card LTG.Dec,
                                                    Int 3))),
                                     Apply (Var "self", Card LTG.Zero))
                       )))) 16)
              @ [LTG.RightApply (16, LTG.Zero)]
    in
      (hd p, tl p)
    end

fun round (_, p) = 
    if null p then (LTG.LeftApply (LTG.I, 0), [])
    else (hd p, tl p)

end
