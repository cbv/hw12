structure DOS :> DOS =
struct

  structure GS = GameState
  structure GA = GrowArray

  exception DOS of string

  (* Index into processes growarray below. *)
  type pid = int
  datatype dos = D of { pid : pid, gs : GS.gamestate }
  datatype dosturn =
      Turn of LTG.turn
    (* If you can't take a turn this round, just return Can'tRun.
       The operating system will take a turn for some other dominator,
       internal operation, or if none can run, idle. *)
    | Can'tRun
  type dominator = { preview : dos -> unit,
                     taketurn : dos -> dosturn }
  datatype process = 
      P of { reserved_slots : int list ref,
             name : string,
             priority : real ref,
             parent : pid option,
             dominator : dominator,
             (* How many virtual cycles has this process has "run", which is
                real turns / priority.  It includes every turn that the
                process was given an opportunity to run, whether or not it
                took that opportunity.  We try to keep the charges of all
                processes about the same. *) 
             charge : real ref
             (* ... *)
             }

  (* The argument that I give to dominators.
     Some internal state is just global. *)
  val reserved = Array.array (256, false)

  fun gamestate (D { gs, ... }) = gs
  fun getpid (D { pid, ... }) = pid

  (* Indexed by pid. *)
  val processes = GA.empty () : process GA.growarray
  fun getpriority (D { pid, ... }) =
      let val P { priority, ... } = GA.sub processes pid
      in !priority
      end
  fun setpriority pid new_priority =
      let val P { priority, ... } = GA.sub processes pid
      in priority := new_priority
      end
  fun getname (D { pid, ... }) =
      let val P { name, ... } = GA.sub processes pid
      in name
      end
  fun getslots (D { pid, ... }) =
      let val P { reserved_slots, ... } = GA.sub processes pid
      in !reserved_slots
      end
  fun longname pid =
      let val P { parent, name, ... } = GA.sub processes pid
      in case parent of
           NONE => name
         | SOME pid => longname pid ^ "." ^ name
      end

  (* PERF: Doesn't need to be linear time. *)
  exception Return of int
  fun reserve_addressable_slot (dos as D { pid, ... })  =
    let 
        val P { reserved_slots, ... } = GA.sub processes pid
        val (_, vitality) = GS.myside (gamestate dos)
    in
        Util.for 0 255
        (fn i => if Array.sub (reserved, Array.sub (Numbers.addressability, i)) orelse
                    (* Maybe should also prefer slots that have higher
                       health, if we don't care about addressability? *)
                    Array.sub (vitality, i) <= 0
                 then ()
                 else (Array.update (reserved, i, true);
                       reserved_slots := i :: !reserved_slots;
                       raise Return i));
        NONE
    end handle Return i => SOME i

  (* We choose to skip the best 64 slots since they are easily
     addressed (unless that's all that's left). *) 
  fun reserve_slot (dos as D { pid, ... })  =
    let 
        val P { reserved_slots, ... } = GA.sub processes pid
        val (_, vitality) = GS.myside (gamestate dos)

        fun try i =
            if Array.sub (reserved, Array.sub (Numbers.addressability, i)) orelse
               (* Maybe should also prefer slots that have higher
                  health, if we don't care about addressability? *)
               Array.sub (vitality, i) <= 0
            then ()
            else (Array.update (reserved, i, true);
                  reserved_slots := i :: !reserved_slots;
                  raise Return i);
    in
        Util.for 64 255 try;
        Util.for 0 63 try;
        NONE
    end handle Return i => SOME i

  fun reserve_fixed_slot (D { pid, ... }) i =
    let val P { reserved_slots, ... } = GA.sub processes pid
    in
      if Array.sub (reserved, i) then false
      else (Array.update (reserved, i, true);
            reserved_slots := i :: !reserved_slots;
            true)
    end

  fun reserve_fixed_slots (D { pid, ... })  l =
    let val P { reserved_slots, ... } = GA.sub processes pid
    in
      if List.exists (fn i => Array.sub (reserved, i)) l
      then false
      else (app (fn i => (Array.update (reserved, i, true);
                          reserved_slots := i :: !reserved_slots)) l;
            true)
    end

  fun release_slot_by_pid pid i =
    let val P { reserved_slots, ... } = GA.sub processes pid
        val rest =
            case ListUtil.extract (fn j => i = j) (!reserved_slots) of 
              SOME (_, rest) => rest
            | NONE => raise DOS ("Releasing slot " ^ Int.toString i ^ " not owned by you")
    in
        if Array.sub (reserved, i)
        then (Array.update (reserved, i, false);
              reserved_slots := rest)
        else raise DOS ("Released unreserved slot " ^ Int.toString i)
    end
  fun release_slot (D { pid, ... }) i = release_slot_by_pid pid i

  fun release_all_slots (dos as D { pid, ... }) = 
    let val P { reserved_slots, ... } = GA.sub processes pid
    in
      app (release_slot dos) (!reserved_slots)
    end

  fun transfer_slot dos {dst, slot} =
    let in
      release_slot dos slot 
      handle DOS _ => raise DOS ("Transfered unreserved slot " ^ Int.toString slot);
      (* This should always succeed *)
      ignore (reserve_fixed_slot dst slot orelse raise DOS ("Failed transfer"))
    end

  fun is_reserved i = Array.sub (reserved, i)

  (* How many game turns have passed. *)
  val turnnum = ref 0
  fun getturnnumber () = !turnnum

  val rtos = Real.fmt (StringCvt.FIX (SOME 2))

  fun spawn parent (name, priority, f) =
      let 
        val total_charge = ref 0.0
        val length = ref 0
        val () = GA.app (fn (P { charge, ... }) => 
                            (total_charge := !total_charge + !charge;
                             length := !length + 1)) 
                        processes
        val charge = if !length = 0 then 0.0 
                     else !total_charge / (Real.fromInt (!length))
        (*
        val () = eprint ("[DOS] new process started with charge " ^
                         rtos (!charge) ^ "\n")
         *)
        val pid = GA.update_next processes (P { reserved_slots = ref nil,
                                                name = name,
                                                priority = ref priority,
                                                parent = parent,
                                                dominator = f,
                                                charge = ref charge})
      in
        pid
      end

  (* A list of processes that have been killed this turn.  They should not
     have another chance to run. *)
  val killed_this_turn = ref []

  fun kill pid =
      let        
        val P { parent, reserved_slots, ... } = GA.sub processes pid
      in
        killed_this_turn := pid :: !killed_this_turn;
        GA.appi (fn (child_pid, P { parent = SOME parent_pid, ... }) => 
                    if parent_pid = pid then kill child_pid else ()
                  | _ => ()) processes;
        (* If we have parent, move reserved_slots to it. Otherwise, free them. *)
        case parent of 
          SOME parent_pid =>
          let val P { reserved_slots = parent_reserved_slots, ... } = 
                  GA.sub processes parent_pid
          in
            parent_reserved_slots := !reserved_slots @ !parent_reserved_slots
          end
        | NONE => app (release_slot_by_pid pid) (!reserved_slots);
        GA.erase processes pid
      end

  fun makelayer (doms : (string * real * dominator) list) =
    let
       (* Starting processes *)
       val () = app (ignore o spawn NONE) doms
       (* TODO: everybody in queue gets to see new state. *)

       val current_pid = ref 0

       fun dos_init _ = ()
       fun dos_taketurn (gs : GS.gamestate) =
         let 
             (* Every dominator gets a preview of its state, and
                might adjust its priority. *)
             val () = GA.appi (fn (pid, P { dominator, ... }) =>
                               let val dos = D { gs = gs, pid = pid }
                               in #preview dominator dos
                               end) processes

             val l = ref nil
             (* Compute the total charge that the process would have if we ran
                it this term.  The favors the higher priority process when two
                processes have the same current charge. *)
             val () = GA.appi (fn (pid, P {charge, priority, ... }) =>
                                  l := (pid, !charge + (1.0 / !priority)) :: !l)
                              processes
             val l = ListUtil.sort (ListUtil.bysecond Real.compare) (!l)

                 (*
             val () =
                 eprint ("[DOS]: Schedulable: " ^ StringUtil.delimit " "
                         (map (fn (pid, charge) =>
                                  (if List.exists (fn p => p = pid) (!killed_this_turn)
                                   then "<killed>" else longname pid)
                                  ^ "@" ^ rtos charge) l) ^ "\n")
                  *)
                 
             fun dosomething nil = LTG.LeftApply (LTG.I, 0) (* Idle! *)
               | dosomething ((pid, new_charge) :: t) = 
                 if List.exists (fn p => p = pid) (!killed_this_turn) then dosomething t else
                 let
                   val P { dominator, charge, ... } = GA.sub processes pid
                   val dos = D { gs = gs, pid = pid }
                 in
                   (* Charge it whether it runs or not. *)
                   charge := new_charge; 
                   case #taketurn dominator dos of
                       Can'tRun => dosomething t
                     | Turn turn =>
                       let in
                         (*
                         eprint ("[DOS] sched: " ^ Int.toString (!turnnum) 
                                 ^ " " ^ (if List.exists (fn p => p = pid) (!killed_this_turn)
                                          then "<killed>" else longname pid) ^ "\n");
                         *)
                         turn
                       end
                 end
         in
             dosomething l 
             before
             (turnnum := !turnnum + 1; 
              killed_this_turn := [])
         end
    in
       (dos_init, dos_taketurn)
    end

end
