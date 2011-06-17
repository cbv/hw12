
structure BogRats :> LAYER =
struct
  open LTG;
  structure GS = GameState

  fun init gs = ()

    (* Returns a sequence of turns that loads the value of integer n
  into slot j *)
  fun loadn n j = 
    let 
      fun load 0 acc = (RightApply (j, Zero)) :: acc
        | load 1 acc = (LeftApply (Succ, j)) :: load 0 acc
	| load n acc = if n mod 2 = 0 then
	                 (LeftApply (Dbl, j)) :: (load (n div 2) acc)
		       else (LeftApply (Succ, j)) :: (load (n - 1) acc)
    in
      rev (load n [LeftApply (Put, j)])
    end


  val target = ref 0
  val targethealth = ref 10000
  val instructions = ref [LTG.RightApply (0, LTG.Zero),
                          LTG.RightApply (1, LTG.Zero)
                          ]
  fun taketurn gs =
    let
      val ins = case (!instructions)    
                 of nil => 
                   (targethealth := (!targethealth) - 1;
                    if (!targethealth) = 0
                    then (target := (!target) + 1; 
                          targethealth := 10000;
                          instructions := (loadn (!target) 0) @ [RightApply(1,Zero),
                                                                 LeftApply (Get,1)];
                          LTG.LeftApply (LTG.Dec, 1))
                    else (instructions := [ RightApply (1,Zero) ,LeftApply (Get,1)];
                          LTG.LeftApply (LTG.Dec, 1))
                      )
                 | ins::inses =>      
                      (instructions := inses;
                       ins)
                
    in
      ins
    end

end

structure Player = LayerFn(BogRats)
