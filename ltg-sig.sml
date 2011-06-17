signature LTG =
sig
  datatype card = 
      Attack
    | Copy
    | Dbl
    | Dec
    | Get
    | Help
    | I
    | Inc
    | K
    | Put
    | Revive
    | S
    | Succ
    | Zero
    | Zombie

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

  datatype value = 
      VFn of function 
    | VInt of int
  datatype exp = 
      App of exp * exp 
    | V of value

  (* Parallel arrays (always size 256) for field and vitality.
     Vitality is always in [-1, 65535]. *)
  type side = value array * int array

  val evalcard : card -> value

  datatype semantics = NORMAL | ZOMBIE

  val initialside : unit -> side
  val initialstate : unit -> side * side

  datatype turn = 
      LeftApply of card * int 
    | RightApply of int * card

  (* Take a turn. Without loss of generality, pass in the proponent as
     the player taking the turn.
     Updates in place. Doesn't check for game-ending
     conditions (all dead; ran out of turns); that should
     be handled by the caller. *)
  val taketurn : side * side -> turn -> unit

end