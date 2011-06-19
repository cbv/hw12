(* The Dominator Operating System, version 3.1. 

   Runs multiple different threads, called dominators,
   simultaneously. A dominator is like a player strategy
   and can do almost anything, but has to reserve the
   slots it's using.
*)

signature DOS =
sig

  type pid
  (* The operating system state, which dominators have
     access to. It may be customized to a particular dominator's
     context, so it may be appropriate to think of this as
     a process id. *)
  type dos

  exception DOS of string

  datatype dosturn =
    (* Make a single regular turn. Your dominator is charged. *)
      Turn of LTG.turn
    (* If you can't take a turn this round, just return Can'tRun.
       The operating system will take a turn for some other dominator,
       internal operation, or if none can run, idle. *)
    | Can'tRun

  (* An individual thread that runs in DOS 3.1.

     preview:
     The preview function is run for every dominator at the beginning
     of the round. It can do things like update internal state or
     change the process's priority, if it really wants to run, or
     wants to kill itself because it's hopeless.

     taketurn:
     Taketurn may run for a dominator if its priority allows it.
     The function might make a regular move, or it might realize
     that it can't progress (still waiting on some ref cell or
     can't reserve a necessary slot, or just has nothing to do).
     If it doesn't do anything, then it is not charged and another
     dominator gets a chance to run. *)
  type dominator = 
      { preview : dos -> unit,
        taketurn : dos -> dosturn }

  (* Get my own pid. *)
  val getpid : dos -> pid
  val getname : dos -> string
  val getpriority : dos -> real

  val setpriority : pid -> real -> unit  

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

  (* Reserve the given slot, if available. Returns true if successful. Will
     allow a slot to be reserved whether or not it is alive. *)
  val reserve_fixed_slot : dos -> int -> bool

  (* Indicate that we no longer need the slot, so it can be
     returned by reserve_*_slot. Doesn't change the contents
     or anything like that. *)
  val release_slot : dos -> int -> unit

  (* Transfer ownership of a slot to |dst|. *)                           
  val transfer_slot : dos -> {dst : dos, slot : int} -> unit

  (* Kill a running dominator. *)
  (* XXX Doesn't free slots or kill children yet *)
  val kill : pid -> unit

  (* Create a new dominator with the given name and priority. If the optional
     parent dominator's pid is supplied, then if the parent is killed,
     so will this dominator be. 

     You should generally use a parent process. The scheduler is not
     good about accounting for unparented processes that start after
     the first round (they can starve other processes). *)
  val spawn : pid option -> string * real * dominator -> pid

  (* Creates the two functions in the LAYER signature by
     scheduling the argument dominators according to its
     policy. Use like this:

     val (init, taketurn) = DOS.makelayer [("Ripens", 1.0, Ripens.create())]

     The first element of each pair is the relative priority of
     that strategy, which can be used by DOS for scheduling. *)
  val makelayer : (string * real * dominator) list ->
      (GameState.gamestate -> unit) * (GameState.gamestate -> LTG.turn)

end
