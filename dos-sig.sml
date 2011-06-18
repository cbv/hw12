(* The Dominator Operating System, version 3.1. 

   Runs multiple different threads, called dominators,
   simultaneously. A dominator is like a player strategy
   and can do almost anything, but has to reserve the
   slots it's using. It can also ask the OS for help
   with various things, like ...?
*)

signature DOS =
sig

  type pid
  (* The operating system state, which dominators have
     access to. It may be customized to a particular dominator's
     context, so it may be appropriate to think of this as
     a process id. *)
  type dos

  datatype dosturn =
(*
     ReserveSlot of  { status: ??? }
     EmitProgram of { turns : LTG.turn list, dst : int, status : ??? }
*)
      Turn of LTG.turn
    (* If you can't take a turn this round, just return Can'tRun.
       The operating system will take a turn for some other dominator,
       internal operation, or if none can run, idle. *)
    | Can'tRun

  (* An individual thread that runs in DOS 3.1 *)
  type dominator = 
      { taketurn : dos -> dosturn }

  (* Get my own pid. *)
  val getpid : dos -> pid
  val getpriority : dos -> real

  val gamestate : dos -> GameState.gamestate

  (* Get a slot that's not currently being used, right now. Tends to be
     a high slot because these are not as useful for the below
     function. The slot may have arbitrary content in it, but won't be
     dead. *)
  val reserve_slot : dos -> int option

  (* Get a slot that's not currently being used, right now. The slot
     is cheap to load as a literal, which are typically slots
     with few 1-bits. These are rarer than the ones returned
     by reserve_slot, so only use this if you need to be able
     to quickly load the slot number as a literal. The slot
     may have arbitrary content in it, but won't be dead. *)
  val reserve_addressable_slot : dos -> int option

  (* Indicate that we no longer need the slot, so it can be
     returned by reserve_*_slot. Doesn't change the contents
     or anything like that. *)
  val release_slot : dos -> int -> unit

  (* Kill a running dominator. *)
  (* XXX Doesn't free slots or kill children yet *)
  val kill : pid -> unit

  (* Create a new dominator with the given priority. If the optional
     parent dominator's pid is supplied, then if the parent is killed,
     so will this dominator be. *)
  val spawn : pid option -> real * dominator -> pid

  (* TODO 
      - add new dominator
      - change priority
     val change_priority : dos -> real -> unit
      - give me a slot that contains a number N
     val slot_with_number : dos -> int -> int opt
      - enable backups
     *)

  (* Creates the two functions in the LAYER signature by
     scheduling the argument dominators according to its
     policy. Use like this:

     val (init, taketurn) = DOS.makelayer [(1.0, Ripens.create())]

     The first element of each pair is the relative priority of
     that strategy, which can be used by DOS for scheduling. *)
  val makelayer : (real * dominator) list ->
      (GameState.gamestate -> unit) * (GameState.gamestate -> LTG.turn)

end
