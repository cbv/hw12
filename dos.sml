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
             priority : real ref,
             parent : pid option,
             dominator : dominator,
             (* How many virtual cycles has this process consumed,
                which is real turns / priority. *)
             charge : real ref
             (* ... *)
             }

  (* The argument that I give to dominators.
     Some internal state is just global. *)
  val reserved = Array.array (256, false)

  fun gamestate (D { gs, ... }) = gs
  fun getpid (D { pid, ... }) = pid

  (* PERF: Doesn't need to be linear time.
     PERF: Should perhaps reserve larger slots if the
     user hasn't asked for an addressable one. *)
  exception Return of int
  fun reserve_slot dos =
    let 
        val (_, vitality) = GS.myside (gamestate dos)
    in
        Util.for 0 255
        (fn i => if Array.sub (reserved, i) orelse
                    (* Maybe should also prefer slots that have higher
                       health, if we don't care about addressability? *)
                    Array.sub (vitality, i) <= 0
                 then ()
                 else (Array.update (reserved, i, true);
                       raise Return i));
        NONE
    end handle Return i => SOME i

  (* TODO: This should be smarter. *)
  val reserve_addressable_slot = reserve_slot

  fun release_slot _ i =
    let in
        if Array.sub (reserved, i)
        then Array.update (reserved, i, false)
        else raise DOS ("Released unreserved slot " ^ Int.toString i)
    end

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

  (* XXX This scheduler is pretty bad! A process can get arbitrarily far
     behind (ahead) and then hog the CPU. Should recenter? *)
  fun getstartcharge _ (* NONE *) priority =
      let
(*
          val min_charge = ref (NONE : real option)
          val () = GA.app (fn (P { charge, ... }) =>
                           case !min_charge of
                               NONE => min_charge := SOME (!charge)
                             | SOME mc => if !charge < mc
                                          then min_charge := SOME (!charge)
                                          else ()) processes

          (* The first process starts with no charge. *)
          val charge = case !min_charge of
              NONE => 0.0
            | SOME c => c
*)
          val max_charge = ref (NONE : real option)
          val () = GA.app (fn (P { charge, ... }) =>
                           case !max_charge of
                               NONE => max_charge := SOME (!charge)
                             | SOME mc => if !charge > mc
                                          then max_charge := SOME (!charge)
                                          else ()) processes

          (* The first process starts with no charge. *)
          val charge = case !max_charge of
              NONE => 0.0
            | SOME c => c
      in
          charge
      end
(*
    | getstartcharge (SOME parent) priority =
      (* Just inherit parent's charge, if it's not a totally new process. *)
      let
          val P { charge, ... } = GA.sub processes parent
      in
          !charge 
      end
*)

  val rtos = Real.fmt (StringCvt.FIX (SOME 2))

  fun spawn parent (priority, f) =
      let 
          val charge = ref (getstartcharge parent priority)
(*
          val () = eprint ("[DOS] new process started with charge " ^
                           rtos (!charge) ^ "\n")
*)
          val pid = GA.update_next processes (P { reserved_slots = ref nil,
                                                  priority = ref priority,
                                                  parent = parent,
                                                  dominator = f,
                                                  charge = charge })
      in
          pid
      end

  fun kill pid =
      let
      in
          (* eprint "XXX NOTE: Kill does not free slots nor kill children, yet!!\n"; *)
          GA.erase processes pid
      end

  fun makelayer (doms : (real * dominator) list) =
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
             val () = GA.appi (fn a => l := a :: !l) processes
             val l = ListUtil.sort (fn ((_, P { charge, ... }),
                                        (_, P { charge = charge', ... })) =>
                                    Real.compare (!charge, !charge')) (!l)

                 (*
             val () =
                 eprint ("[DOS]: Schedulable: " ^ StringUtil.delimit " "
                         (map (fn (pid, P { charge, ... }) =>
                               Int.toString pid ^ "@" ^ rtos (!charge)) l) ^ "\n")
                 *)

             fun dosomething nil = LTG.LeftApply (LTG.I, 0) (* Idle! *)
               | dosomething (((pid, P { dominator, charge, 
                                         priority, ... })) :: t) = 
                 let val dos = D { gs = gs, pid = pid }
                 in
                   case #taketurn dominator dos of
                       Can'tRun => dosomething t
                     | Turn turn =>
                           (* Only move the one that took the move
                              to the tail. *)
                           let in
                               charge := !charge + (1.0 / !priority);
                               turn
                           end
                 end
         in
             dosomething l
         end
    in
       (dos_init, dos_taketurn)
    end

end
