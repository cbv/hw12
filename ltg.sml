(* Imperative implementation of Lambda The Gathering games. *)
structure LTG :> LTG =
struct

  val application_count_hook : (int -> unit) option ref = ref NONE

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

  val DEBUG = false

  val lastmsg = ref ""
  val eprint =
      fn s => if s = !lastmsg
              then ()
              else (TextIO.output (TextIO.stdErr, "[LTG] " ^ s ^ "\n"); lastmsg := s)


  fun initialstat () = { left_applications = ref 0,
                         right_applications = ref 0,
                         damage_done = ref 0.0,
                         healing_done = ref 0.0,
                         iterations = ref 0,
                         gotten = ref 0 }

  fun initialstats () = Vector.tabulate (256, fn _ => initialstat ())

  fun statfor s i = Vector.sub (s, i)

  fun stat_left_applications (s : stat) = ! (#left_applications s)
  fun stat_right_applications (s : stat) = ! (#right_applications s)
  fun stat_damage_done (s : stat) = ! (#damage_done s)
  fun stat_healing_done (s : stat) = ! (#healing_done s)
  fun stat_iterations (s : stat) = ! (#iterations s)
  fun stat_gotten (s : stat) = ! (#gotten s)

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

  fun valtos (VInt i) = Int.toString i
    | valtos (VFn f) = ftos f
  and ftos f =
   (case f of
       VI => "I"
     | VSucc => "Succ"
     | VDbl => "Dbl"
     | VGet => "Get"
     | VPut => "Put"
     | VS nil => "S"
     | VS l => "(S " ^ StringUtil.delimit " " (rev (map valtos  l)) ^ ")"
     | VK nil => "K"
     | VK l => "(K " ^ StringUtil.delimit " " (rev (map valtos l)) ^ ")"
     | VInc => "Inc"
     | VDec => "Dec"
     | VAttack nil => "Attack"
     | VAttack l => "(Attack " ^ StringUtil.delimit " " (rev (map valtos l)) ^ ")"
     | VHelp nil => "Help"
     | VHelp l => "(Help " ^ StringUtil.delimit " " (rev (map valtos l)) ^ ")"
     | VCopy => "Copy"
     | VRevive => "Revive"
     | VZombie nil => "Zombie"
     | VZombie l => "(Zombie " ^ StringUtil.delimit " " (rev (map valtos l)) ^ ")")

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
     
  fun incby NONE _ _ _ = ()
    | incby (SOME r) i f n = (f (Vector.sub (r, i))) += n
  fun incbyr NONE _ _ _ = ()
    | incbyr (SOME r) i f n = (f (Vector.sub (r, i))) +== n

  fun inc NONE _ _ = ()
    | inc (SOME r) i f = ++ (f (Vector.sub (r, i)))
  fun incr r i f = incbyr r i f 1.0

  val tracing = ref false
  fun enable_trace b = tracing := b

  fun clamp n = if n > 65535
                then 65535
                else n

  fun isdead vit = vit <= 0

  datatype semantics = NORMAL | ZOMBIE

  exception EvalError of string and EvalLimit
  fun expectslotnumber (VInt n) =
      if n >= 0 andalso n <= 255
      then n else raise EvalError ("Bad slot number " ^ Int.toString n)
    | expectslotnumber _ = raise EvalError ("Slot number non-numeric")

  (* Evaluate an expression, returning its
     value. May raise exception EvalError. May
     have side effects. *)
  fun evalwithstate semantics (((propf, propv) : side, propstats : stats option), 
                               ((oppf,  oppv) : side, oppstats : stats option))
                    (* Only used for collecting stats. *)
                    (curslot : int)
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
                 inc propstats curslot #iterations;
                 case v1 of
                   VInt i => raise EvalError ("int " ^ Int.toString i ^
                                              " in application position")
                 | VFn f =>
                  (case f of
                     VI => v2
                   | VSucc =>
                      (case v2 of
                           VInt n => VInt (clamp (n + 1))
                         | _ => raise EvalError "argument to succ not int")
                   | VDbl =>
                      (case v2 of
                           VInt n => VInt (clamp (n * 2))
                         | _ => raise EvalError "argument to dbl not int")
                   | VGet => 
                      let val i = expectslotnumber v2
                          val vit = Array.sub(propv, i)
                      in if isdead vit
                         then raise EvalError "get on dead slot"
                         else 
                             let in
                                 inc propstats i #gotten;
                                 Array.sub(propf, i)
                             end
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
                                     NORMAL =>
                                      let in
                                          incr propstats curslot #healing_done;
                                          Array.update (propv, i, clamp (vit + 1))
                                      end
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
                                     NORMAL => 
                                         let in
                                             if !tracing
                                             then eprint ("DEC " ^ Int.toString i ^
                                                          "\n")
                                             else ();
                                             incr propstats curslot #damage_done;
                                             Array.update (oppv, oppi, vit - 1)
                                         end
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
                                         then raise EvalError "attack too big"
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
                                     let in
                                         (* XXX, this assumes we are not
                                            in zombie semantics. *)
                                         if semantics = NORMAL
                                         then 
                                             let in
                                                 if DEBUG
                                                 then eprint ("Did " ^ Int.toString (vito - newvito) ^
                                                              " damage!")
                                                 else ();
                                                 incbyr propstats curslot 
                                                   #damage_done (real (vito - newvito))
                                             end
                                         else ();
                                         Array.update (oppv, oppj, newvito)
                                     end);
                                VFn VI
                            end
                      | _ => raise EvalError "attack arg not int")
                   | VAttack l => VFn (VAttack (v2 :: l))
                   | VHelp [j, i] =>
                       (case v2 of
                            VInt n =>
                                let val i = expectslotnumber i

                                    val vitp = Array.sub (propv, i)
                                    val () = if n > vitp
                                             then raise EvalError "help too big"
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
                                            if semantics = NORMAL
                                            then 
                                                let in
                                                    if DEBUG
                                                    then eprint ("Did " ^ Int.toString (newwitp - witp) ^
                                                                 " healing!")
                                                    else ();
                                                    incbyr propstats curslot 
                                                      #healing_done 
                                                      (real (newwitp - witp))
                                                end
                                            else ();
                                            Array.update (propv, j, newwitp)
                                          end);
                                    VFn VI
                                end
                          | _ => raise EvalError "help arg not int")
                   | VHelp l => VFn (VHelp (v2 :: l))
                   | VCopy =>
                       let val i = expectslotnumber v2
                       in Array.sub (oppf, i)
                       end
                   | VRevive =>
                       let val i = expectslotnumber v2
                           val vit = Array.sub (propv, i)
                       in (if isdead vit
                           then 
                               let in
                                   (* Does increment health. Maybe should
                                      be a separate counter too though? *)
                                   incr propstats curslot #healing_done;
                                   Array.update (propv, i, 1)
                               end
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
                            else raise EvalError "zombie on living slot");
                           VFn VI
                       end
                   | VZombie l => VFn (VZombie (v2 :: l)))
               end

      val rv = eval exp
      val _ = (case application_count_hook of ref(SOME f) => f(!steps) | _ => ())
    in
        rv
    end

  fun initialside () = (Array.array (256, VFn VI),
                        Array.array (256, 10000))
  fun initialstate () = (initialside (), initialside ())

  datatype halfturn =
      HLeftApply of card 
    | HRightApply of card

  datatype turn =
      LeftApply of card * int
    | RightApply of int * card

  fun halfturn2turn i (HLeftApply c) = LeftApply (c, i)
    | halfturn2turn i (HRightApply c) = RightApply (i, c)

  exception SlotOutOfBoundsOrDead

  fun turn2str (LeftApply (c, i))
    = "(" ^ Card.card2str c ^ ", " ^ Int.toString i ^ ")"
    | turn2str (RightApply (i, c))
    = "(" ^ Int.toString i ^ ", " ^ Card.card2str c ^ ")"

  fun turns2str (t :: nil) = turn2str t
   |  turns2str ts = foldl (fn (t, s) => s ^ " " ^ turn2str t) "" ts

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
                                                     (opp, NONE)) 
                                             j zombie)
                       handle EvalError s => ()
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
                                             (opp, oppstats)) i init
                       handle EvalError s => 
                           let in
                               if DEBUG orelse !tracing
                               then eprint ("Evaluation ended with error: " ^ s)
                               else ();
                               VFn VI
                           end
                            | EvalLimit => 
                           let in
                               if DEBUG orelse !tracing
                               then eprint ("Evaluation exceeded limits")
                               else();
                               VFn VI
                           end
               in
                   if !tracing
                   then eprint ("Result of eval:\n" ^
                                valtos result ^ "\n")
                   else ();
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

  fun slotisdead (_, vitalities) i = Array.sub(vitalities, i) <= 0

end
