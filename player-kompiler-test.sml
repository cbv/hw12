structure Player :> PLAYER =
struct
open Kompiler

type state = LTG.turn list

fun init _ = 
    let 
      (* Put the program "fn x => 7" into slot 13 and then call it. *)
      val p = (compile (Lambda("x", Int 7)) 13) 
              @ [LTG.RightApply (13, LTG.Zero)]
    in
      (hd p, tl p)
    end

fun round (_, p) = 
    if null p then (LTG.LeftApply (LTG.I, 0), [])
    else (hd p, tl p)

end
