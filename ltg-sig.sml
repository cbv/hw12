signature LTG =
sig
  datatype card = datatype Card.card

  (* Keeps various counters for the slots. *)
  type stat
  type stats

  val initialstats : unit -> stats

  (* Expensive! Don't call except when in debugging modes. *)
  val statstostring : stats -> string

  (* Get the stat in the slot, which must be in [0, 255] *)
  val statfor : stats -> int -> stat

  (* Accessors for the stat fields.
     TODO(tom7): document *)
  val stat_left_applications : stat -> int
  val stat_right_applications : stat -> int
  val stat_damage_done : stat -> real
  val stat_healing_done : stat -> real
  val stat_iterations : stat -> int
  val stat_gotten : stat -> int

  (* All possible primitive functions, including partial 
     applications. For partial applications, the argument
     are stored in reverse order in the list. Probably
     don't need to use these, but they can be inspected
     as part of a strategy, for example. *)
  datatype function = 
      VAttack of value list
    | VCopy
    | VDbl
    | VDec
    | VGet
    | VHelp of value list
    | VI
    | VInc
    | VK of value list
    | VPut
    | VRevive
    | VS of value list
    | VSucc
    | VZombie of value list

  and value = 
      VFn of function 
    | VInt of int

  datatype exp = 
      App of exp * exp 
    | V of value

  val valtos : value -> string
  val ftos : function -> string

  (* Parallel arrays (always size 256) for field and vitality.
     Vitality is always in [-1, 65535]. *)
  type side = value array * int array

  val evalcard : card -> value

  datatype semantics = NORMAL | ZOMBIE

  (* If tracing is enabled, then print out steps of evaluation. *)
  val enable_trace : bool -> unit

  val initialside : unit -> side
  val initialstate : unit -> side * side

  val slotisdead : side -> int -> bool

  datatype turn = 
      LeftApply of card * int 
    | RightApply of int * card

  val turn2str : turn -> string
  val turns2str : turn list -> string

  (* Take a turn. Without loss of generality, pass in the proponent as
     the player taking the turn.
     Updates in place. Doesn't check for game-ending
     conditions (all dead; ran out of turns); that should
     be handled by the caller. *)
  val taketurn : side * side -> turn -> unit

  (* Same, but optionally accumulate stats. *)
  val taketurnex : (side * stats option) * (side * stats option) -> turn -> unit

end
