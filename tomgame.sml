(* Imperative implementation of games. *)
structure Game =
struct

  (* Cards are atomic actions *)
  datatype card =
     I
   | Zero
   | Succ
   | Dbl
   | Get
   | Put
   | S
   | K
   | Inc
   | Dec
   | Attack
   | Help
   | Copy
   | Revive
   | Zombie

  (* All possible primitive functions, including partial 
     applications. For partial applications, the argument
     are stored in reverse order in the list. *)
  datatype function =
     VI
   | VSucc
   | VDbl
   | VGet
     (* n.b. appears to allow partial application,
        but the partial application is always equivalent
        to VI, so we just use that. *)
   | VPut
   | VS of value list
   | VK of value list
   | VInc
   | VDec
   | VAttack of value list
   | VHelp of value list
   | VCopy
   | VRevive
   | VZombie of value list

  and value =
     (* Always in [0, 65535]. *)
     VInt of int
   | VFn of function

  datatype exp =
     App of exp * exp
   | V of value

  (* Parallel arrays (always size 256) for field and vitality.
     Vitality is always in [-1, 65535]. *)
  type side = value Array.array * int Array.array

  type game = side * side

  fun evalcard c =
      case c of
     I => VFn VI
   | Zero => VInt 0
   | Succ => VFn VSucc
   | Dbl => VFn VDbl
   | Get => VFn VGet
   | Put => VFn VPut
   | S => VFn (VS nil)
   | K => VFn (VK nil)
   | Inc => VFn VInc
   | Dec => VFn VDec
   | Attack => VFn (VAttack nil)
   | Help => VFn (VHelp nil)
   | Copy => VFn VCopy
   | Revive => VFn VRevive
   | Zombie => VFn (VZombie nil)

  fun clamp n = if n > 65535
                then 65535
                else n

  fun isdead vit = vit <= 0

  datatype semantics = NORMAL | ZOMBIE

  exception EvalError and EvalLimit
  fun expectslotnumber (VInt n) =
      if n >= 0 andalso n <= 255
      then n else raise EvalError
    | expectslotnumber _ = raise EvalError

  (* Evaluate an expression, returning its
     value. May raise exception EvalError. May
     have side effects. *)
  fun evalwithstate semantics ((propf, propv) : side, 
                               (oppf,  oppv) : side) exp : value =
    let
      (* XXX Definitely a potential for off-by-one errors here.
         Need to validate against the reference simulator? *)
      val steps = ref 0
      fun step () =
          let val s = !steps + 1
          in if s > 1000
             then raise EvalLimit
             else steps := s
          end

      fun eval (exp : exp) : value =
          case exp of
              V v => v (* VVV *)
            | App (e1, e2) =>
               let val v1 = eval e1
                   val v2 = eval e2
               in
                 (* XXX don't know where this goes, because
                    the spec is not totally clear on what
                    constitutes a "function call." for example,
                    does it go before the recursive calls? *)
                 step ();
                 case v1 of
                   VInt _ => raise EvalError
                 | VFn f =>
                  (case f of
                     VI => v2
                   | VSucc =>
                      (case v2 of
                           VInt n => VInt (clamp (n + 1))
                         | _ => raise EvalError)
                   | VDbl =>
                      (case v2 of
                           VInt n => VInt (clamp (n * 2))
                         | _ => raise EvalError)
                   | VGet => 
                      let val i = expectslotnumber v2
                          val vit = Array.sub(propv, i)
                      in if isdead vit
                         then raise EvalError
                         else Array.sub(propf, i)
                      end
                   | VPut => VFn VI
                   | VS [g, f] =>
                       let val x = v2
                          (* n.b., some subtlety to the
                             evaluation order because of
                             effects and errors, but we
                             believe this matches the
                             spec. - tom and william. *)
                       in eval (App (App (V f, V x),
                                     App (V g, V x)))
                       end
                   | VS l => VFn (VS (v2 :: l))
                   | VK [x] => x
                   | VK l => VFn (VK (v2 :: l))
                   | VInc =>
                       let val i = expectslotnumber v2
                           val vit = Array.sub(propv, i)
                       in (if isdead vit
                           then ()
                           else (case semantics of
                                     NORMAL => Array.update (propv, i,
                                                             clamp (vit + 1))
                                   | ZOMBIE => Array.update (propv, i, vit - 1)));
                           VFn VI
                       end
                   | VDec => 
                       let val i = expectslotnumber v2
                           val oppi = 255 - i
                           val vit = Array.sub(oppv, oppi)
                       in (if isdead vit
                           then ()
                           else (case semantics of
                                     NORMAL => Array.update (oppv, oppi, vit - 1)
                                   | ZOMBIE => Array.update (oppv, oppi,
                                                             clamp (vit + 1))));
                           VFn VI
                       end
                   | VAttack [j, i] =>
                       (case v2 of
                            VInt n =>
                                let val i = expectslotnumber i

                                    val vitp = Array.sub (propv, i)
                                    val () = if n > vitp
                                             then raise EvalError
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
                                     else Array.update (oppv, oppj, newvito));
                                    VFn VI
                                end
                          | _ => raise EvalError)
                   | VAttack l => VFn (VAttack (v2 :: l))
                   | VHelp [j, i] =>
                       (case v2 of
                            VInt n =>
                                let val i = expectslotnumber i

                                    val vitp = Array.sub (propv, i)
                                    val () = if n > vitp
                                             then raise EvalError
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
                                     else Array.update (propv, j, newwitp));
                                    VFn VI
                                end
                          | _ => raise EvalError)
                   | VHelp l => VFn (VHelp (v2 :: l))
                   | VCopy =>
                       let val i = expectslotnumber v2
                       in Array.sub (oppf, i)
                       end
                   | VRevive =>
                       let val i = expectslotnumber v2
                           val vit = Array.sub (propv, i)
                       in (if isdead vit
                           then Array.update (propv, i, 1)
                           else ());
                          VFn VI
                       end
                   | VZombie [i] =>
                       let val i = expectslotnumber i
                           val oppi = 255 - i
                           val vito = Array.sub (oppv, oppi)
                       in
                           (if isdead vito
                            then (Array.update (oppv, oppi, ~1);
                                  Array.update (oppf, oppi, v2))
                            else raise EvalError);
                           VFn VI
                       end
                   | VZombie l => VFn (VZombie (v2 :: l)))
               end
    in
        eval exp
    end

  fun initialside () = (Array.array (255, VFn VI),
                        Array.array (255, 10000))
  fun initialstate () = (initialside (), initialside ())

  datatype turn =
      LeftApply of card * int
    | RightApply of int * card

  fun aliveslot (propf, propv) i =
      if i < 0 orelse i > 255 orelse isdead (Array.sub (propv, i))
      then raise EvalError
      else Array.sub (propf, i)

  (* Take a turn. Without loss of generality, pass in the proponent as
     the player taking the turn. *)
  fun taketurn (prop as (propf, propv), opp) turn =
      let 
          (* Figure out what expression we're going to evaluate, and
             get the index so that we can write back to it. *)
          val (init, i) =
          case turn of
              LeftApply (c, i) => (App (V (evalcard c), V (aliveslot prop i)), i)
            | RightApply (i, c) => (App (V (aliveslot prop i), V (evalcard c)), i)

          (* Do the auto-application to zombies, which is anything
             that has vitality -1. *)
          fun auto j =
              if Array.sub (propv, j) = ~1
              then
                  let val zombie = App (V (Array.sub (propf, j)), V (VFn VI))
                  in
                      (* Just evaluating for effects. *)
                      (ignore (evalwithstate ZOMBIE (prop, opp) zombie)
                       handle EvalError => ()
                            | EvalLimit => ());
                      (* Always reset to identity and regular-dead. *)
                      Array.update (propf, j, VFn VI);
                      Array.update (propv, j, 0)
                  end
              else ()

          val () = Util.for 0 255 auto

          (* Evaluate the expression normally. If we have to stop because 
             we ran out of iterations or encountered some error, then we fill
             the slot with the identity. Otherwise it gets the
             resulting value from evaluation. *)
          val result = 
              evalwithstate NORMAL (prop, opp) init
              handle EvalError => VFn VI
                   | EvalLimit => VFn VI
      in
          Array.update (propf, i, result)
      end

end
