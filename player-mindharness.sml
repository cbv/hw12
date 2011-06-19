
structure MindHarness :> LAYER =
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


  val helpinc = 128

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

  fun helpableslot (values, vitalities) = 
      let 
          val vlist = List.tabulate (256, fn i => (i,Array.sub (vitalities,i)))
          val livevlist = List.filter (fn x => #2 x > helpinc  ) vlist
      in
          if List.null livevlist
          then NONE
          else let val (low, lowvalue) = ListUtil.min compare_healths livevlist
               in if lowvalue <= 0 then SOME (low) else NONE
               end
      end

  fun findreviverslot side = 
      let val vitalities = #2 side
          val vlist = List.tabulate (252, fn i => (i,Array.sub (vitalities,i + 4)))
          val (max, _) = ListUtil.max compare_healths vlist
      in
          max
      end

 
 (* find subexpressions of the form "Help n" and "Attack n" appear in v
*)

  fun tallysupportslots acc v = 
      case v
       of VFn (VAttack [VInt n]) => n::acc
        | VFn (VAttack [v, VInt (n)]) => tallysupportslots (n::acc) v
        | VFn (VHelp [VInt (n)]) => n::acc
        | VFn (VHelp [v, VInt (n)]) => tallysupportslots (n::acc) v
        | VFn (VK [v]) => tallysupportslots acc v
        | VFn (VS [v]) => tallysupportslots acc v
        | VFn (VS [v1,v2]) => tallysupportslots (tallysupportslots acc v1) v2
        | _ => acc


  fun findattackslot values = 
      let val valuelist = List.tabulate (255, fn i => Array.sub(values,i))
          val sss = List.map (tallysupportslots nil) valuelist
          val ss = List.concat sss
      in
          (* XXX think a little bit more *)
          if List.null ss then NONE else SOME(List.hd ss)
      end

  fun printside (values, vitalities) = 
      let 
          val _ = eprint "values :\n"
          val _ = Util.for 0 255 (fn i => eprint ((valtos ` Array.sub(values, i) ) ^ " "))
          val _ = eprint "\n" 
          val _ = eprint "supportslots :\n"
          val _ = Util.for 0 255 (fn i => (List.map 
                                              (fn x => eprint ` Int.toString x ^ " " )
                                              (tallysupportslots [] (Array.sub(values, i)));
                                           eprint ", "))
          val _ = eprint "\n" 
          val _ = eprint "vitalities :\n"
          val _ = Util.for 0 255 (fn i => eprint ((Int.toString ` Array.sub(vitalities, i) ) ^ " "))
          val _ = eprint "\n"
      in () end



  (* applies f[1] to f[0], putting result in f[1] *)
  val applyregs = [LeftApply (K, 1),
                   LeftApply (S, 1),
                   RightApply (1, Get),
                   RightApply (1, Zero)]


  (* assumes the attacker is in slot 2 *)
  fun attackslot s mine = 
      (compile (Int 2) 1) @   (* first load the attacker *)
      [LeftApply (Get, 1)] @
      (compile (Int mine) 0) @  
      applyregs @
      (compile (Int s) 0) @ 
      applyregs

  (* assumes the helper is in slot 3 *)
  fun helperizeslot n = 
      (compile (Int 3) 1) @   (* first load the attacker *)
      [LeftApply (Get, 1)] @
      (compile (Int n) 0) @  
      applyregs 

  val attacker = \ "x" (\ "y"  ( Card (Attack)  -- $"x" -- $"y" -- (Int 2048)  )   ) 

  val helper = 
      fix ` \ "f" ` \ "n" ( Card S -- Card Help --  Card I -- 
                                 $"n" -- Int helpinc -- ( $"f" -- $"n" ) )


  val opening = (compile attacker 2)

  val buildhelper = ( compile helper 3 )

  val buildhelperins = ref buildhelper

  val helperbuilt = ref false

  val oldinscontext = ref []
  val instructions = ref opening


  val n = ref 0;
  val reviving = ref false;

  val _ = eprint "this is mindharness\n"
(*  val _ = eprint ((Int.toString ` List.length (compile helper1 0)) ^ "\n")
  val _ = eprint ((Int.toString ` List.length (compile helper2 0)) ^ "\n")
*)




  fun updateinstructions gs = 
      let
          val myside = GS.myside gs
          val theirside = GS.theirside gs
          val (theirvalues, theirvitalities) = theirside
(*         val _ = eprint ` Int.toString (!n) ^ ":\n"
         (*  val _ = printside myside *)
          val _ = printside theirside   *)
          val stats = GS.theirstats gs
          val slots = List.tabulate (256, fn i =>
                                             (i, LTG.statfor stats i))
          val slots = map (scoreslot theirside) slots
          val reviverslot = findreviverslot myside
          val (best, _) = ListUtil.max compare_scores slots
          val ((high, highval), ( low, lowval)) = findhighlowhealth myside
          val helpthisone = case helpableslot myside 
                             of NONE => high
                              | SOME(x) => x
      in case (finddeadslot myside, !reviving)
          of (SOME(dead), false) =>
           ( reviving := true;
             oldinscontext := !instructions;
             instructions := (compile (Int dead) reviverslot) @ 
                             [LeftApply (Revive, reviverslot)]
           )
             | _ => 
               if List.null (!instructions)    
               then (  case findattackslot theirvalues
                        of NONE => 
                           (case !buildhelperins
                             of ins::inses => (instructions := [ins]; buildhelperins := inses)
                              | nil => ( helperbuilt := true;
                                         instructions := helperizeslot helpthisone)
                                         
                           )
                         | SOME(s) => (
                           instructions := attackslot (255 - s)  high
                           )
                    )
               else ()
      end




  fun init gs =  ()
  fun taketurn gs =
      let
          val () = n := !n + 1
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
          ins
      end


end

structure Player = LayerFn(MindHarness)
