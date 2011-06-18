
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



  val myslot0 = 1
  val myslot1 = 2
  val helpamount = 2048
  val hurtamount = 512
  val targetpointer = 4


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
                   
                      
  val target = ref 0

  fun init gs = ()

  fun taketurn gs =
      let
          val theirside = GS.theirside gs
          val stats = GS.theirstats gs
(*          val health = Array.sub (#2 theirside, 255 - !target) *)
          val slots = List.tabulate (256, fn i =>
                                             (i, LTG.statfor stats i))
          val slots = map (scoreslot theirside) slots
                      
          val (best, _) = ListUtil.max compare_scores slots

(*          val _ = if health <= 0 
                  then target := (!target) + 1 
                  else () *)
          val () = if List.null (!instructions)    
                   then instructions := ((compile (Int (255 - best)) 0) @ (copyregs2 1) @ applyregs)
                   else ()
          val (ins,inses) = (List.hd (!instructions), List.tl (!instructions))
          val _ = instructions := inses
      in
          ins
      end


end

structure Player = LayerFn(Leviathan)
