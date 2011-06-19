structure AbstractEval :> ABSTRACTEVAL = 
struct

open LTG;



  datatype unknownvalue =
    Unknown
  (* UnknownInt(n) is greater than or equal to n *)
  | UnknownInt of int
  (* UnknownSlotValue(n) is loaded from a slot having id at least n *)
  | UnknownSlotValue of int
  (* ... ? *)

  fun unknownvalue2str u = 
      case u of
          Unknown => "unknown"
        | UnknownInt (i) => "unknownint(" ^ Int.toString i ^ ")"
        | UnknownSlotValue(i) => "unknownslotvalue(" ^ Int.toString i ^ ")"

  datatype abstractfunction =
     AVI
   | AVSucc
   | AVDbl
   | AVGet
   | AVPut
   | AVS of abstractvalue list
   | AVK of abstractvalue list
   | AVInc
   | AVDec
   | AVAttack of abstractvalue list
   | AVHelp of abstractvalue list
   | AVCopy
   | AVRevive
   | AVZombie of abstractvalue list

  and abstractvalue =
     (* Always in [0, 65535]. *)
     AVInt of int
   | AVFn of abstractfunction
   | AVUnknown of unknownvalue

  fun abstractfunction2str f = 
      case f of
          AVI => "I"
        | AVSucc => "Succ"
        | AVDbl => "Dbl"
        | AVGet => "Get"
        | AVPut => "Put"
        | AVS(l) => "(S " ^ StringUtil.delimit " " (map abstractvalue2str  l) ^ ")"
        | AVK(l) =>  "(K " ^ StringUtil.delimit " " (map abstractvalue2str  l) ^ ")"
        | AVInc => "Inc"
        | AVDec => "Dec"
        | AVAttack(l) => "(Attack " ^ StringUtil.delimit " " (map abstractvalue2str l) ^ ")"
        | AVHelp(l) => "(Help " ^ StringUtil.delimit " " (map abstractvalue2str l) ^ ")"
        | AVCopy => "Copy"
        | AVRevive => "Revive"
        | AVZombie(l) => "(Zombie " ^ StringUtil.delimit " " (map abstractvalue2str l) ^ ")"

  and abstractvalue2str v =
      case v of
          AVInt(i) => Int.toString(i)
        | AVFn(f) => abstractfunction2str f
        | AVUnknown u => unknownvalue2str u

  datatype abstractexp =
     AApp of abstractexp * abstractexp
   | AV of abstractvalue


  datatype effect = 
    EAttack of abstractvalue list
  | EHelp of abstractvalue list
  | EInc of abstractvalue
  | EDec of abstractvalue
  | ERevive of abstractvalue
  | EZombie of abstractvalue list


  fun effect2str e = 
      case e of 
          EAttack(l) => "(Attack " ^ StringUtil.delimit " " (map abstractvalue2str l) ^ ")"
        | EHelp(l) => "(Help " ^ StringUtil.delimit " " (map abstractvalue2str l) ^ ")"
        | EInc(v) => "(Inc " ^ abstractvalue2str v ^ ")"
        | EDec(v) => "(Dec " ^ abstractvalue2str v ^ ")"
        | ERevive(v) => "(Revive " ^ abstractvalue2str v ^ ")"
        | EZombie(l) => "(Zombie " ^ StringUtil.delimit " " (map abstractvalue2str l) ^")"



  fun concrete2abstractfunction (f: function) : abstractfunction = 
      case f 
       of VI => AVI
        | VSucc => AVSucc
        | VDbl => AVDbl
        | VGet => AVGet
        | VPut => AVPut
        | VS ls => AVS (List.map concrete2abstractvalue ls)
        | VK ls => AVK (List.map concrete2abstractvalue ls)
        | VInc => AVInc
        | VDec => AVDec
        | VAttack ls => AVAttack (List.map concrete2abstractvalue ls)
        | VHelp ls => AVHelp (List.map concrete2abstractvalue ls)
        | VCopy => AVCopy
        | VRevive => AVRevive
        | VZombie ls => AVZombie (List.map concrete2abstractvalue ls)

  and concrete2abstractvalue (v: value) : abstractvalue = 
      case v 
       of VInt n => AVInt n
        | VFn f => AVFn (concrete2abstractfunction f)

  val abstractify = concrete2abstractvalue

  fun clamp n = if n > 65535
                then 65535
                else n


  fun isdead vit = vit <= 0

  val tracing = ref false
  fun enable_trace b = tracing := b

  exception AbsEvalError of string and AbsEvalLimit
  fun expectslotnumber (AVInt n) =
      if n >= 0 andalso n <= 255
      then n else raise AbsEvalError ("Bad slot number " ^ Int.toString n)
    | expectslotnumber _ = raise AbsEvalError ("Slot number non-numeric")



  (* Evaluate an expression, returning its
     value. May raise exception AbsEvalError. May
     have side effects. *)
  fun evalwithstate semantics (((propf, propv) : side), 
                                ((oppf,  oppv) : side))
                    (exp : abstractexp) : (abstractvalue option * effect list) =
    let
      (* XXX Definitely a potential for off-by-one errors here.
         Need to validate against the reference simulator? *)
      val steps = ref 0
      fun step () =
          let val s = !steps + 1
          in if s > 1000
             then raise AbsEvalLimit
             else steps := s
          end

      val effects = ref nil
      fun addeffect(e) = effects := e :: (!effects)
          
      fun eval (exp : abstractexp) : abstractvalue =
          case exp of
              AV v => v (* VVV *)
            | AApp (e1, e2) =>
               let val v1 = eval e1
                   val v2 = eval e2
               in
                 (* XXX don't know where this goes, because
                    the spec is not totally clear on what
                    constitutes a "function call." for example,
                    does it go before the recursive calls? *)
                 step ();
                 case v1 of
                     AVInt i => raise AbsEvalError ("int " ^ Int.toString i ^
                                                    " in application position")
                   | AVFn f =>
                     (case f of
                          AVI => v2
                        | AVSucc =>
                          (case v2 of
                               AVInt n => AVInt (clamp (n + 1))
                             | AVUnknown (UnknownInt n) => AVUnknown (UnknownInt(clamp (n + 1)))
                             | AVUnknown u => AVUnknown (UnknownInt 1)
                             | _ => raise AbsEvalError "argument to succ not int")
                        | AVDbl =>
                          (case v2 of
                               (AVInt n) => (AVInt (clamp (n * 2)))
                             | AVUnknown (UnknownInt n) => AVUnknown (UnknownInt(clamp (n * 2)))
                             | AVUnknown u => AVUnknown (UnknownInt 0)
                             | _ => raise AbsEvalError "argument to dbl not int")
                        | AVGet => (
                          case v2
                           of (AVInt _) => 
                              let val i = expectslotnumber v2
                                  val vit = Array.sub(propv, i)
                              in if isdead vit
                                 then raise AbsEvalError "get on dead slot"
                                 else concrete2abstractvalue (Array.sub(propf, i)) 
                              end
                            | (AVUnknown u) => 
                                 AVUnknown (UnknownSlotValue 0)
                            | _ => raise AbsEvalError "bad get"
                          )
                        | AVPut => AVFn AVI
                        | AVS [g, f] =>
                          let val x = v2
                          (* n.b., some subtlety to the
                             evaluation order because of
                             effects and errors, but we
                             believe this matches the
                             spec. - tom and william. *)
                          in eval (AApp (AApp (AV f, AV x),
                                         AApp (AV g, AV x)))
                          end
                        | AVS l => (AVFn (AVS (v2 :: l)))
                        | AVK [x] => x
                        | AVK l => AVFn (AVK (v2 :: l))
                        | AVInc => (addeffect(EInc v2); AVFn AVI)
                        | AVDec => (addeffect(EDec v2); AVFn AVI )
                        | AVAttack [j, i] => ( addeffect(EAttack [v2,j,i]);
                                               AVFn AVI 
                                             )
                        | AVAttack l => ( (* addeffect( EAttack (v2::l)); *)
                                         AVFn (AVAttack (v2 :: l)))
                        | AVHelp [j, i] => (addeffect  (EHelp [v2,j,i]);
                                            AVFn AVI )
                        | AVHelp l => ( (* addeffect(EHelp (v2::l)) ; *)
                                        AVFn (AVHelp (v2 :: l)))
                        | AVCopy => (
                          case v2 of
                              (AVInt _) => 
                              let val i = expectslotnumber v2
                                  val vit = Array.sub(oppv, i)
                              in if isdead vit
                                 then raise AbsEvalError "get on dead slot"
                                 else concrete2abstractvalue (Array.sub(oppf, i)) 
                              end
                            | (AVUnknown u) => 
                                 AVUnknown (UnknownSlotValue 0)
                            | _ => raise AbsEvalError "bad get"
                          )
                        | AVRevive => (addeffect(ERevive v2); AVFn AVI)
                        | AVZombie [i] => (addeffect(EZombie [v2, i] ); AVFn AVI)
                        | AVZombie l => AVFn (AVZombie (v2 :: l)))
                   | _ => AVUnknown Unknown
               end
      val retexp = SOME(eval exp )   handle AbsEvalError s => NONE
                                          | AbsEvalLimit => NONE;
    in
        (retexp, !effects)
    end


  fun evalwithstate1 d semantics (prop : side, 
                                (opp : side))
                      (exp : abstractexp) : (effect list) =
      let  fun aux exp1 depth acc = 
              if depth = 0 then acc else
               case exp1 of
                   AV (AVFn AVI) => acc
                 | AV (AVInt i) => acc
                 | AV (AVUnknown u) => acc
                 | _ =>  ( let val newexp = AApp (exp1, AV (AVUnknown Unknown))
                           in
                               case evalwithstate semantics (prop,opp) newexp of
                                   (NONE, es) => es @ acc
                                 | (SOME(av), es) => aux (AV av) (depth - 1) (es @ acc )
                           end
                         )
      in
          aux exp d nil
      end


end
