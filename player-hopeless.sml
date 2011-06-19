structure Player:> PLAYER = struct

   type state = unit

   fun init _ = raise Match

   fun round _ = (LTG.LeftApply (LTG.I, 0), ())

end
