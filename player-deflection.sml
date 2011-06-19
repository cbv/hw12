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


  fun array2list a =
      List.tabulate(Array.length a, fn i => Array.sub(a,i))


  val n = ref 0

  fun doevals gs = 
      let
          val myside = GS.myside gs
          val theirside = GS.theirside gs
          val (values, vitalities) = theirside
          val vlist = array2list(values)
      in
          case List.find iscode vlist of
              SOME(code) => (
              let val _ = eprint ` Int.toString (!n) ^ ": "
                  val _ = eprint "found some code\n"
                  val _ = eprint ` valtos code ^ "\n"
                  val abscode = abstractify code
                  val newcode = AApp (AV abscode, AV ` AVUnknown Unknown )
                  val (resval, reseffects) =
                      evalwithstate NORMAL (theirside, myside) newcode
                  val _ = eprint `
                           StringUtil.delimit ", " (map effect2str reseffects)
                  val _ = eprint "\n"
              in () end
              )         
            | NONE => ()          
      end


 
  fun init gs =  ()
  fun taketurn gs =
      let
          val () = n := !n + 1
          val myside = GS.myside gs
          val theirside = GS.theirside gs
          val _ = doevals gs
      in
          RightApply(0, I)
      end


end

structure Player = LayerFn(Deflection)
