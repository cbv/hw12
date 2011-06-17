structure Player :> PLAYER =
struct
open Kompiler

type state = LTG.turn list

fun init _ = 
    let 
      val p = (compile (Lambda("x", Int 7)) 13) 
              @ [LTG.RightApply (13, LTG.Zero)]
    in
      (hd p, tl p)
    end

fun round (_, p) = 
    if null p then (LTG.LeftApply (LTG.I, 0), [])
    else (hd p, tl p)

end
