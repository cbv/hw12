structure AbstractEval :> ABSTRACTEVAL = 
struct

open LTG;

  datatype abstractvalue =
           AbstractValue 
         | AbstractInt of int
         | Concrete of LTG.value

  datatype abstractexp =
     AbsApp of abstractexp * abstractexp
   | AV of abstractvalue



  datatype abstractfunction =
     AVI
   | AVSucc
   | AVDbl
   | AVGet
     (* n.b. appears to allow partial application,
        but the partial application is always equivalent
        to VI, so we just use that. *)
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
   | AVUnknown

  datatype abstractexp =
     AApp of abstractexp * abstractexp
   | AV of abstractvalue


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
  fun evalwithstate2 semantics (((propf, propv) : side), 
                                ((oppf,  oppv) : side))
                    (exp : abstractexp) : abstractvalue =
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
                             | _ => raise AbsEvalError "argument to succ not int")
                        | AVDbl =>
                          (case v2 of
                               (AVInt n) => (AVInt (clamp (n * 2)))
                             | _ => raise AbsEvalError "argument to dbl not int")
                        | AVGet => 
                          let val i = expectslotnumber v2
                              val vit = Array.sub(propv, i)
                          in if isdead vit
                             then raise AbsEvalError "get on dead slot"
                             else concrete2abstractvalue (Array.sub(propf, i)) 
                          end
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
                   | AVInc =>
                       let val i = expectslotnumber v2
                           val vit = Array.sub(propv, i)
                       in (if isdead vit
                           then ()
                           else (case semantics of
                                     NORMAL =>
                                      let in
                                          Array.update (propv, i, clamp (vit + 1))
                                      end
                                   | ZOMBIE => Array.update (propv, i, vit - 1)));
                           AVFn AVI
                       end
                   | AVDec => 
                       let val i = expectslotnumber v2
                           val oppi = 255 - i
                           val vit = Array.sub(oppv, oppi)
                       in (if isdead vit
                           then ()
                           else (case semantics of
                                     NORMAL => 
                                         let in
                                             if !tracing
                                             then eprint ("DEC " ^ Int.toString i ^
                                                          "\n")
                                             else ();
                                             Array.update (oppv, oppi, vit - 1)
                                         end
                                   | ZOMBIE => Array.update (oppv, oppi,
                                                             clamp (vit + 1))));
                           AVFn AVI
                       end
                   | AVAttack [j, i] =>
                       (case v2 of
                        AVInt n =>
                            let val i = expectslotnumber i

                                val vitp = Array.sub (propv, i)
                                val () = if n > vitp
                                         then raise AbsEvalError "attack too big"
                                         else ()
                                val () = Array.update (propv, i, vitp - n);

                                val j = expectslotnumber j
                                val oppj = 255 - j
                                (* aka w *)
                                val vito = Array.sub (oppv, oppj)
                                val dmg = (n * 9) div 10
                                val newvito = 
                                    (case semantics of
                                         NORMAL =>
                                             if dmg > vito
                                             then 0
                                             else vito - dmg
                                       | ZOMBIE => 
                                             clamp (vito + dmg))
                            in
                                (if isdead vito
                                 then ()
                                 else 
                                     Array.update (oppv, oppj, newvito)
                                    );
                                AVFn AVI
                            end
                      | _ => raise AbsEvalError "attack arg not int")
                   | AVAttack l => AVFn (AVAttack (v2 :: l))
                   | AVHelp [j, i] =>
                       (case v2 of
                            AVInt n =>
                                let val i = expectslotnumber i

                                    val vitp = Array.sub (propv, i)
                                    val () = if n > vitp
                                             then raise AbsEvalError "help too big"
                                             else ()
                                    val () = Array.update (propv, i, vitp - n)

                                    val j = expectslotnumber j
                                    (* aka w *)
                                    val witp = Array.sub (propv, j)
                                    val heal = (n * 11) div 10
                                    val newwitp = 
                                        (case semantics of
                                             NORMAL => clamp (witp + heal)
                                           | ZOMBIE => 
                                               if heal > witp
                                               then 0
                                               else witp - heal)
                                in
                                    (if isdead witp
                                     then ()
                                     else let in
                                            Array.update (propv, j, newwitp)
                                          end);
                                    AVFn AVI
                                end
                          | _ => raise AbsEvalError "help arg not int")
                   | AVHelp l => AVFn (AVHelp (v2 :: l))
                   | AVCopy =>
                       let val i = expectslotnumber v2
                       in AVUnknown (* Array.sub (oppf, i) *)
                       end
                   | AVRevive =>
                       let val i = expectslotnumber v2
                           val vit = Array.sub (propv, i)
                       in (if isdead vit
                           then 
                               let in
                                   (* Does increment health. Maybe should
                                      be a separate counter too though? *)
                                   Array.update (propv, i, 1)
                               end
                           else ());
                          AVFn AVI
                       end
                   | AVZombie [i] =>
                       let val i = expectslotnumber i
                           val oppi = 255 - i
                           val vito = Array.sub (oppv, oppi)
                       in
                           (if isdead vito
                            then (Array.update (oppv, oppi, ~1) ;
                                  eprint "oops\n"
(*                                  Array.update (oppf, oppi, v2 )  *))  
                            else raise AbsEvalError "zombie on living slot");
                           AVFn AVI
                       end
                   | AVZombie l => AVFn (AVZombie (v2 :: l)))
                   | _ => raise AbsEvalError "unimplemented"
               end
    in
        eval exp
    end


end
