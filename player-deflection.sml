structure Deflection :> LAYER =
struct
  open LTG;
  open Kompiler;
  open AbstractEval;

  structure GS = GameState


  infix 9 --
  val op -- = Apply
  val $ = Var
  fun \ x exp = Lambda (x, exp)
  infixr 1 `
  fun a ` b = a b



  fun printside (values, vitalities) = 
      let 
          val _ = eprint "values :\n"
          val _ = Util.for 0 255 (fn i => eprint ((valtos ` Array.sub(values, i) ) ^ " "))
          val _ = eprint "\n" 
          val _ = eprint "vitalities :\n"
          val _ = Util.for 0 255 (fn i => eprint ((Int.toString ` Array.sub(vitalities, i) ) ^ " "))
          val _ = eprint "\n"
      in () end


  fun iscode(v) = 
      case v of
          VFn(VI) => false
        | VFn(f) => true
        | VInt(i) => false

  fun isnumber(v) = 
      case v of
          VInt(i) => true
        | _ => false


  fun array2list a =
      List.tabulate(Array.length a, fn i => Array.sub(a,i))


  val n = ref 0

  fun doevals gs = 
      let
          val myside = GS.myside gs
          val theirside = GS.theirside gs
          val (values, vitalities) = theirside
          val vlist = array2list(values)
          fun aux code = (
              let 
                  val _ = eprint "found some code\n"
                  val _ = eprint ` valtos code ^ "\n"
                  val _ = eprint "it has the following effects\n" 
                  val abscode = AV ` abstractify code 
                  val reseffects =
                      evalwithstate1 30 NORMAL (theirside, myside) abscode
                  val _ = eprint `
                                 StringUtil.delimit ", " (map effect2str reseffects)
                  val _ = eprint "\n"
              in () end
              )
          val codes = List.filter iscode vlist
          val _ = map aux codes
          val _ = eprint "\n"
      in
          ()
      end

  fun examinenumbers gs = 
      let
          fun printnum (i, v) = case v of
                               VInt n => eprint ` Int.toString i ^ ": " ^ Int.toString n  ^ ", "
                             | _ => ()
          val myside = GS.myside gs
          val theirside = GS.theirside gs
          val (values, vitalities) = theirside
          val _ = eprint "numbers:\n"
          val _ = Array.appi printnum values  
          val _ = eprint "\n"
      in () end
 

  fun dohelp helperslot j = 
    []
  

  fun buildapplier slot = 
      [LeftApply (K, slot),
       LeftApply (S, slot),
       RightApply (slot, Get) ]

  (* assume that the slot starts with I *)
  fun copyapplier slot = 
      [RightApply (slot, Zero), 
       LeftApply (Succ, slot),
       LeftApply (Get, slot)]

  val prog1 =  \ "x" ` Card (Help) -- (Card Get -- (Int 2)) -- (Card Get -- (Int 4)) -- $"x"

  val opening = 
      (Macros.fastnum 0 8192) @
      ( compile prog1 1) @
      (buildapplier 1) @
      (copyapplier 3) @
      (Macros.fastnum 2 1) @ 
      (Macros.fastnum 4 0)


  val nextthing = 
      [RightApply (3, Zero)] @
      (copyapplier 3) @
      [ LeftApply (Succ,2),
        LeftApply (Succ,4)]

  val instructions  = ref opening

  fun countins prog = List.length (compile prog 0)


  val _ = eprint ` "opening inses: " ^ Int.toString (List.length opening) ^ "\n"


  val reviving = ref false

  fun updateinstructions gs = 
      let
          val myside = GS.myside gs
          val theirside = GS.theirside gs
          val (theirvalues, theirvitalities) = theirside
      in 
          if List.null (!instructions)    
          then instructions := nextthing
          else ()
      end



  fun init gs =  ()
  fun taketurn gs =
      let
          val () = n := !n + 1
          val _ = eprint ` "\nturn " ^ Int.toString (!n) ^ ": "
          val myside = GS.myside gs
          val theirside = GS.theirside gs
(*          val _ = printside myside *)
          val _ = doevals gs
          val _ = examinenumbers gs
          val () = updateinstructions gs
          val (ins,inses) = (List.hd (!instructions), List.tl (!instructions))
          val _ = instructions := inses
          val _ = case ins 
                   of LeftApply (Revive, n) => ()
                      (* reviving := false;
                        instructions := (!oldinscontext)
                      *)
                    | _ =>  ()
      in
          ins
      end


end

structure Player = LayerFn(Deflection)
