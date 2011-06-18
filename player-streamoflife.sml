
structure StreamOfLife :> LAYER =
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
  val compare_healths = ListUtil.bysecond Int.compare 
    

  (* We're going to want to transfer life from the slot with the most
     life to the slot with the least.
     XXX? In case of tie, favor the lower numbers.
   *)
  fun findhighlowhealth side = 
      let val vitalities = #2 side
          val vlist = List.tabulate (256, fn i => (i,Array.sub (vitalities,i)))
          val (low, lowval) = ListUtil.min compare_healths vlist
          val (high, highval) = ListUtil.max compare_healths vlist
      in
          ((high,highval), (low,lowval))
      end

  fun finddeadslot side = 
      let val vitalities = #2 side
          val vlist = List.tabulate (256, fn i => (i,Array.sub (vitalities,i)))
          val (low, lowvalue) = ListUtil.min compare_healths vlist
      in
          if lowvalue <= 0 then SOME (low) else NONE
      end

  fun findreviverslot side = 
      let val vitalities = #2 side
          val vlist = List.tabulate (253, fn i => (i,Array.sub (vitalities,i + 3)))
          val (max, _) = ListUtil.max compare_healths vlist
      in
          max
      end

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

(* transfer vitality from i to j *) 
  fun help i j = 
     (compile (Int i) 1) @ 
     [LeftApply (Help, 1)] @ 
     (compile (Int j) 0) @
     applyregs @
     (compile (Int 2) 0) @ 
     [LeftApply (Get, 0)] @ 
     applyregs



  val builtnum = ref 8192;

  fun buildnum n put =
      (if put then [LeftApply(Put,n)] else [] ) @
      [ RightApply(n,Zero),
        LeftApply(Succ,n), (* 1 *)
        LeftApply(Dbl,n),
        LeftApply(Dbl,n),
        LeftApply(Dbl,n),
        LeftApply(Dbl,n),
        LeftApply(Dbl,n),  (* 32 *)
        LeftApply(Dbl,n),  (* 64 *)
        LeftApply(Dbl,n),  (* 128 *)
        LeftApply(Dbl,n),  (* 256 *)
        LeftApply(Dbl,n),  (* 512 *)
        LeftApply(Dbl,n),  (* 1024 *)
        LeftApply(Dbl,n),  (* 2048 *)
        LeftApply(Dbl,n),  (* 4096 *)
        LeftApply(Dbl,n)  (* 8192 *)
      ]


  val inithelps = (help 6 0) @ 
                   (help 8 1) @
                   (help 9 2) @
                   (help 10 0) @
                   (help 12 1) @
                   (help 16 2)
  



  val oldinscontext = ref ` help 0 0

  val instructions = ref ` (buildnum 2 false  ) @ inithelps
                   


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
          val reviverslot = findreviverslot myside
          val (best, _) = ListUtil.max compare_scores slots
          val ((high, highval), ( low, lowval)) = findhighlowhealth myside
      in case (finddeadslot myside, !reviving)
          of (SOME(dead), false) =>
           ( reviving := true;
             oldinscontext := !instructions;
             instructions := (compile (Int dead) reviverslot) @ 
                             [LeftApply (Revive, reviverslot)]
           )
             | _ => 
               if List.null (!instructions)    
               then ( (* eprint "restarting instructions\n";  *)
                   if highval < !builtnum
                   then (builtnum := Int.div(highval, 2) ;
                         instructions := ( compile (Int (! builtnum)) 2 )
                       )
                   else 
                     instructions := help high low 
                   )
               else ()
      end


  fun taketurn gs =
      let
          val () = updateinstructions gs
          val (ins,inses) = (List.hd (!instructions), List.tl (!instructions))
          val _ = instructions := inses
          val _ = case ins 
                   of LeftApply (Revive, n) => 
                      ( reviving := false;
                        instructions := (!oldinscontext)
                      )
                    | _ =>  ()
      in
(*          eprint ` turn2str ins ^ "\n"; *)
          ins
      end


end

structure Player = LayerFn(StreamOfLife)
