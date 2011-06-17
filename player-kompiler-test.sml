structure Player :> PLAYER =
struct
structure K = Kompiler
datatype src = datatype K.src

type state = LTG.turn list

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

          (* Also, run Dec forever to check that we can loop. *)
          (* XXX buggy! *)
          @ (K.compile (K.fix (Lambda ("self", 
                                Lambda ("_", 
                                 Apply (Apply (Card LTG.K, Var "self"),
                                         (Apply (Card LTG.Dec, Int 3))
                       ))))) 15)
              @ [LTG.RightApply (14, LTG.Zero)]
    in
      (hd p, tl p)
    end

(*
                                               (K.Card LTG.Put,
                                                (K.Apply (K.Card LTG.Dec,
                                                          K.Int 3))),
                                              K.Var "self"
 *)
 
fun round (_, p) = 
    if null p then (LTG.LeftApply (LTG.I, 0), [])
    else (hd p, tl p)

end
