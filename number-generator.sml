structure NumberGenerator :> NUMBER_GENERATOR =
struct
structure GS = GameState

exception NumberGenerator of string

datatype status
  = NotDone
  | Done of int

(* States:
    - no slot: get a slot
    - got slot and src is SOME: run the compiler
    - src is NONE and turns is not empty: run turns
    - no turns: done
*)
type work = {caller_dos : DOS.dos,
             slot : int option ref,
             src : Kompiler.src option ref,
             turns : LTG.turn list ref, 
             status : status ref}
val queue = ref [] : work list ref

(* Keep track of whether or not someone has started us. *)
datatype global_status 
  = NotRunning
  | WaitingForTimecuve of EmitProgram.status
  | Running 
val global_status = ref NotRunning

(* Hard code some useful values?
 val known_valuable_numbers = [31, 63, 127, 8192]
 *)

fun create () =
    let
      fun preview dos = global_status := Running
(*
 XXX Don't build the timecube until later in the game.
          case !global_status of
            NotRunning => 
            let
              if DOS.reserve_fixed_slot 2 then
              val (stat, dom) = EmitProgram.emit 
                                    (Kompiler.compile Timecube.timecube slot)
              val pid = DOS.spawn (SOME (DOS.getpid dos)) 
                                  (0.1 * (DOS.getpriority dos), dom)
            in

            end
            | _ => ()
*)
      fun taketurn dos =
          let in
            case !queue of
              nil => DOS.Can'tRun
            | {caller_dos, slot, src, turns, status} :: rest =>
              (case !slot of
                 NONE => (* No slot yet *)
                 ((* eprint ("NG: no slot yet\n"); *)
                  slot := DOS.reserve_addressable_slot caller_dos;
                  case !slot of
                    NONE => DOS.Can'tRun (* Didn't get one; pass. *)
                  | _ => taketurn dos) (* Got one; continue. *)
               | SOME slot => (* We have a slot! *)
                 (case !src of
                    SOME src' => (* If there is src, compile it. *)
                    ((* eprint ("NG: compiling\n"); *)
                     turns := Kompiler.compile (src') slot;
                     src := NONE;
                     taketurn dos)
                  | NONE =>
                    (case !turns of
                       nil => raise (NumberGenerator "No turns in work?!?")
                     | (t :: ts) => (* Play the turns. *)
                       ((* eprint ("NG: playing turn\n"); *)
                        if null ts then (* Did the last one, so clean up. *)
                          (status := Done slot;
                           queue := rest)
                        else 
                          turns := ts;
                        DOS.Turn t))))
          end
    in
      {preview = preview, taketurn = taketurn}
    end

fun generate caller_dos goal =
    case !global_status of NotRunning =>
      raise NumberGenerator "You must start the number generator service."
    | _ =>
      let 
        (* val () = eprint ("NG: got request for " ^ Int.toString goal ^ "\n") *)
        val gs = DOS.gamestate caller_dos
        val (values, vitality) = GS.myside gs

        val status = ref NotDone
        (* These are encodings of the work left to do, if any. *)
        val slot = ref NONE
        val src = ref NONE
        val turns = ref nil

        (* Checks if the slot is live and available; reserves it. *)
        fun check_and_reserve i = 
            Array.sub (vitality, i) >= 0
            andalso DOS.reserve_fixed_slot caller_dos i

        (* Returns the sequence of cards needed to make the given value into
           the goal, if that sequence is short.  Otherwise, NONE. *)
        (* XXX We should compare how costly it would be to compute the number
          in other ways, e.g. we might be willing to take something that
          requires 3 cards for 255, but only 2 cards for 3. *)
        fun is_close_to_goal i = 
            let
              val v = (Array.sub (values, i))
            in
              case v of 
                LTG.VInt v =>
                (if v = goal andalso check_and_reserve i then
                   SOME nil
                 else if v + 1 = goal andalso check_and_reserve i then 
                   SOME [Card.Succ]
                 else if v * 2 = goal andalso check_and_reserve i then 
                  SOME [Card.Dbl]
                 else if v + 2 = goal andalso check_and_reserve i then 
                   SOME [Card.Succ, Card.Succ]
                 else if v * 4 = goal andalso check_and_reserve i then
                   SOME [Card.Dbl, Card.Dbl]
                 else NONE)
              |  _ => NONE
            end

        (* Used to exit loops below.*)
        exception Found
      in
        (* Let's figure out how we are going to do this.*)
        (* 1. Look for existing cells with (nearly) the right value. *)
        Util.for 0 255 
          (fn i => case is_close_to_goal i
                    of SOME cards => 
                       (slot := SOME i;
                        turns := map (fn c => LTG.LeftApply (c, i)) cards;
                        if null cards then status := Done i else ();
                        eprint ("NG: Found close match in slot " ^ Int.toString i
                                ^ " for value " ^ Int.toString goal ^ "\n");
                        raise Found)
                     | _ => ())
            handle Found => ();

        (* Other strategies ....
            Timecube,
            Precomputed ones,
            etc.
         *)

        (* n. Otherwise, run the code to build the number in a new cell. *)
        if !status = NotDone andalso null (!turns) then
          (eprint ("NG: Compiling new source for value " ^ Int.toString goal ^ "\n");
           src := SOME (Kompiler.Int goal))
        else ();

        (* If there is work to do, enqueue it. *)
        case !status of Done _ => ()
                      | _ =>
        (queue := ({caller_dos = caller_dos,
                    slot = slot,
                    src = src,
                    turns = turns,
                    status = status} :: !queue));
        (* Return the status. *)
        status
      end

end
