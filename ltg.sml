(* Imperative implementation of Lambda The Gathering games. *)
structure LTG :> LTG =
struct

  (* TODO: Zombies? Not clear how to account
     for a zombie since it may be my own property... *)
  type stat = { left_applications : int ref,
                right_applications : int ref,
                damage_done : real ref,
                healing_done : real ref,
                iterations : int ref,
                gotten : int ref }
  (* Using vector since every field of stat is mutable. *)
  type stats = stat Vector.vector

  fun initialstat () = { left_applications = ref 0,
                         right_applications = ref 0,
                         damage_done = ref 0.0,
                         healing_done = ref 0.0,
                         iterations = ref 0,
                         gotten = ref 0 }

  fun initialstats () = Vector.tabulate (256, fn _ => initialstat ())

  (* Cards are atomic actions *)
  datatype card = datatype Card.card

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

  fun ++ r = r := !r + 1
  infix += +==
  fun r += (n : int) = r := !r + n
  fun r +== (d : real) = r := !r + d

  fun inc NONE _ _ = ()
    | inc (SOME r) i f = ++ (f (Vector.sub (r, i)))
      
  fun incby NONE _ _ _ = ()
    | incby (SOME r) i f n = (f (Vector.sub (r, i))) += n
  fun incbyr NONE _ _ _ = ()
    | incbyr (SOME r) i f n = (f (Vector.sub (r, i))) +== n

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
  fun evalwithstate semantics (((propf, propv) : side, propstats : stats option), 
                               ((oppf,  oppv) : side, oppstats : stats option))
                    (exp : exp) : value =
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

  fun initialside () = (Array.array (256, VFn VI),
                        Array.array (256, 10000))
  fun initialstate () = (initialside (), initialside ())

  datatype turn =
      LeftApply of card * int
    | RightApply of int * card

  exception SlotOutOfBoundsOrDead

  fun turn2str (LeftApply (c, i))
    = "(" ^ Card.card2str c ^ ", " ^ Int.toString i ^ ")"
    | turn2str (RightApply (i, c))
    = "(" ^ Int.toString i ^ ", " ^ Card.card2str c ^ ")"

  fun turns2str (t :: nil) = turn2str t
   |  turns2str ts = foldl (fn (t, s) => turn2str t ^ " " ^ s) "" ts

  fun aliveslot (propf, propv) i =
      if i < 0 orelse i > 255 orelse isdead (Array.sub (propv, i))
      then raise SlotOutOfBoundsOrDead
      else Array.sub (propf, i)

  (* Take a turn. Without loss of generality, pass in the proponent as
     the player taking the turn. *)
  fun taketurnex ((prop as (propf, propv), propstats : stats option),
                  (opp, oppstats : stats option)) turn =
      let 
          (* Do the auto-application to zombies, which is anything
             that has vitality -1. *)
          (* TODO: Do we want to keep stats on zombie work? *)
          fun auto j =
              if Array.sub (propv, j) = ~1
              then
                  let val zombie = App (V (Array.sub (propf, j)), V (VFn VI))
                  in
                      (* Just evaluating for effects. Not computing
                         stats. *)
                      (ignore (evalwithstate ZOMBIE ((prop, NONE), 
                                                     (opp, NONE)) zombie)
                       handle EvalError => ()
                            | EvalLimit => ());
                      (* Always reset to identity and regular-dead. *)
                      Array.update (propf, j, VFn VI);
                      Array.update (propv, j, 0)
                  end
              else ()

          (* PERF: Can keep this as a bitmask/list of zombies, so that we
             don't have to loop over every slot every turn? *)
          val () = Util.for 0 255 auto


          (* Figure out what expression we're going to evaluate, and
             get the index so that we can write back to it. Also updates
             the stats if the turn was legal. 

             TODO: Could increment contempt in the case that the opponent
             tries to execute a dead slot, because there should be no
             reason to ever do that (we have exact info about the
             vitality of each slot). *)
          fun getturn turn =
              (case turn of
                   LeftApply (c, i) => 
                       let 
                           val f = aliveslot prop i
                       in
                           inc propstats i #left_applications;
                           SOME (App (V (evalcard c), V f), i)
                       end
                 | RightApply (i, c) => 
                       let 
                           val f = aliveslot prop i
                       in
                           inc propstats i #right_applications;
                           SOME (App (V f, V (evalcard c)), i)
                       end)
                   handle SlotOutOfBoundsOrDead => NONE
      in
          case getturn turn of
              NONE => ()
            | SOME (init, i) =>
               let
                   (* Evaluate the expression normally. If we have to stop because 
                      we ran out of iterations or encountered some error,
                      then we fill the slot with the identity.
                      Otherwise it gets the resulting value from
                      evaluation. *)
                   val result = 
                       evalwithstate NORMAL ((prop, propstats), 
                                             (opp, oppstats)) init
                       handle EvalError => VFn VI
                            | EvalLimit => VFn VI
               in
                   Array.update (propf, i, result)
               end
      end

  fun taketurn (prop, opp) turn = taketurnex ((prop, NONE), (opp, NONE)) turn


  val itos = Int.toString
  val rtos = Real.fmt (StringCvt.FIX (SOME 2))

  fun statstostring stats =
    let
        fun stattostring {left_applications : int ref,
                          right_applications : int ref,
                          damage_done : real ref,
                          healing_done : real ref,
                          iterations : int ref,
                          gotten : int ref } =
              "la: " ^ itos (!left_applications) ^
              " ra: " ^ itos (!right_applications) ^
              " dmg: " ^ rtos (!damage_done) ^
              " heal: " ^ rtos (!healing_done) ^
              " it: " ^ itos (!iterations) ^
              " got: " ^ itos (!gotten)

        fun isdefault { left_applications = ref 0,
                        right_applications = ref 0,
                        iterations = ref 0,
                        gotten = ref 0, 
                        (* can't damage or heal without
                           iterations. *)
                        damage_done = _,
                        healing_done = _ } = true
          | isdefault _ = false
                        

        val l = Vector.foldri 
            (fn (idx, stat, l) =>
             if isdefault stat
             then l
             else (("[" ^ itos idx ^ "] " ^ 
                    stattostring stat) :: l)) nil stats
    in
        StringUtil.delimit "\n" l
    end

end
