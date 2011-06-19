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
 
  fun init gs =  ()
  fun taketurn gs =
      let
          val () = n := !n + 1
          val _ = eprint ` "\nturn " ^ Int.toString (!n) ^ ": "
          val myside = GS.myside gs
          val theirside = GS.theirside gs
          val _ = doevals gs
          val _ = examinenumbers gs
      in
          RightApply(0, I)
      end


end

structure Player = LayerFn(Deflection)
