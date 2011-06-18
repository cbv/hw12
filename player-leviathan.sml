
structure Leviathan :> LAYER =
struct
  open LTG;
  open Kompiler;

  structure GS = GameState


  infix 9 --
  val op -- = Apply
  val $ = Var
  fun \ x exp = Lambda (x, exp)
  infixr 1 `
  fun a ` b = a b





  (* Maybe should have a lower bound on what it will
     consider valuable, and just heal/revive if there
     are no current high-value targets. *)
  fun scoreslot side (idx : int, s : LTG.stat) =
      (idx,
       (* XXX weighted! *)
       if LTG.slotisdead side idx
       then ~1000.0
       else real (LTG.stat_left_applications s) +
            real (LTG.stat_right_applications s) +
            LTG.stat_damage_done s +
            LTG.stat_healing_done s +
            real (LTG.stat_iterations s) +
            real (LTG.stat_gotten s))

  val compare_scores = ListUtil.bysecond Real.compare 
    

  val fnslot = 0
  val target = ref 0

  (* applies f[1] to f[0], putting result in f[1] *)
  val applyregs = [LeftApply (K, 1),
                   LeftApply (S, 1),
                   RightApply (1, Get),
                   RightApply (1, Zero)]

  (* applies (S f[1]) to f[0], putting result in f[1] *)
  val sapplyregs =  LeftApply(S,1)  :: applyregs
                   

  (* copies f[1] to f[n] *)
  fun copyregs1 n = [LeftApply (Put, n),
                    RightApply (n, Zero),
                    LeftApply (Succ, n),
                    LeftApply (Get, n)]

  (* copies f[2] to f[n] *)
  fun copyregs2 n = [LeftApply (Put, n),
                     RightApply (n, Zero),
                     LeftApply (Succ, n),
                     LeftApply (Succ, n),
                     LeftApply (Get, n)]


  val builddecker =  [RightApply (0, Dec),
                      RightApply (1, Dec)]
                     @ sapplyregs (* 2 damage  *)
                     @ ( copyregs1 0)
                     @ sapplyregs (* 4 damage  *)
                     @ ( copyregs1 0)
                     @ sapplyregs (* 8 damage  *)
                     @ ( copyregs1 0)
                     @ sapplyregs
                     @ ( copyregs1 0)
                     @ sapplyregs
                     @ ( copyregs1 0)
                     @ sapplyregs
                     @ ( copyregs1 0)
                     @ sapplyregs
                     @ ( copyregs1 0)
                     @ sapplyregs (*256 damage *)
                     @ ( copyregs1 0)
                     @ sapplyregs (*512 damage *)
                    (* once it's constructed, move it to f[2] *)
                     @ ( copyregs1 2)

  val instructions = ref builddecker                   
                   

(*  fun someliveslot myside = 
        

 *)
      
                      
  val target = ref 0

  fun init gs = ()

  val reviving = ref false

  fun updateinstructions gs = 
      let
          val myside = GS.myside gs
          val theirside = GS.theirside gs
          val stats = GS.theirstats gs
          val slots = List.tabulate (256, fn i =>
                                             (i, LTG.statfor stats i))
          val slots = map (scoreslot theirside) slots
          val scratch = 69
          val (best, _) = ListUtil.max compare_scores slots
      in
          if slotisdead myside 0 andalso not (!reviving)
          then ( reviving := true;
              instructions := 
               [LeftApply (Put, scratch),
                RightApply (scratch, Zero),
                LeftApply (Revive, scratch)])
          else  ( if slotisdead myside 1 andalso not (!reviving)
                  then ( reviving := true;
                         instructions := 
                         [LeftApply (Put, scratch),
                          RightApply (scratch, Zero),
                          LeftApply (Succ, scratch),
                          LeftApply (Revive, scratch)] ) 
                  else (
                      if slotisdead myside 2 andalso not (!reviving)
                      then ( reviving := true;
                             instructions := 
                             [LeftApply (Put, scratch),
                              RightApply (scratch, Zero),
                              LeftApply (Succ, scratch),
                              LeftApply (Succ, scratch),
                              LeftApply (Revive, scratch)] )
                      else (
                          if List.null (!instructions)    
                          then instructions := 
                               ((compile (Int (255 - best)) 0) @ (copyregs2 1) @ applyregs)
                          else () ) ) )
      end


  fun taketurn gs =
      let
          val () = updateinstructions gs
          val (ins,inses) = (List.hd (!instructions), List.tl (!instructions))
          val _ = instructions := inses
          val _ = case ins 
                   of LeftApply (Revive, n) => reviving := false
                    | _ =>  ()
      in
          ins
      end


end

structure Player = LayerFn(Leviathan)
