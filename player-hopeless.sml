structure Player:> PLAYER = struct
 
   type state = unit

   fun init _ = (LTG.LeftApply (LTG.I, 0), ())

   fun round _ = (LTG.LeftApply (LTG.I, 0), ())

end
