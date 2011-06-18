
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
(*
  val compare_scores = ListUtil.bysecond Real.compare 
  val compare_healths = ListUtil.bysecond Int.compare 
  *)  



  val opening = [
      RightApply(0, Zero)
  ]


  val n = ref 0;


  fun init gs = ()
  fun taketurn gs =
      let
          val theirside = GS.theirside gs
          val vitalities = #2 theirside
          val values = #1 theirside
          val () = n := !n + 1
          val _ = eprint ` Int.toString (!n) ^ ":\n"
          val _ = eprint "values :\n"
          val _ = Util.for 0 255 (fn i => eprint ((valtos ` Array.sub(values, i) ) ^ " "))
          val _ = eprint "\n" 
          val _ = eprint "vitalities :\n"
          val _ = Util.for 0 255 (fn i => eprint ((Int.toString ` Array.sub(vitalities, i) ) ^ " "))
          val _ = eprint "\n"
      in
          RightApply(0,Zero)
      end


end

structure Player = LayerFn(MindHarness)
