signature ABSTRACTEVAL =
sig


  datatype unknownvalue =
    Unknown
  (* UnknownInt(n) is greater than or equal to n *)
  | UnknownInt of int
  (* UnknownSlotValue(n) is loaded from a slot having id at least n *)
  | UnknownSlotValue of int
  (* ... ? *)

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
   | AVUnknown of unknownvalue


  datatype abstractexp =
     AApp of abstractexp * abstractexp
   | AV of abstractvalue


  datatype effect = 
    EAttack of abstractvalue list
  | EHelp of abstractvalue list
  | EInc of abstractvalue
  | EDec of abstractvalue
  | ERevive of abstractvalue
  | EZombie of abstractvalue


  val abstractvalue2str : abstractvalue -> string
  val effect2str : effect -> string 

  val abstractify : LTG.value -> abstractvalue

  (* If tracing is enabled, then print out steps of evaluation. 
   ( XXX doesn't actually right now... )  *)
  val enable_trace : bool -> unit


(* computes (abstract) effects of side1 evaluating this expression *)
  val evalwithstate :  LTG.semantics -> LTG.side * LTG.side -> abstractexp 
                       -> (abstractvalue option * effect list)

(* computes (abstract) effects of side1 repeatedly evaluating this expression 
  until we get an atomic value.
  first argument is limit to how many times to repeat
 *)
  val evalwithstate1 :  int -> LTG.semantics -> LTG.side * LTG.side -> abstractexp 
                        -> (effect list)

end
