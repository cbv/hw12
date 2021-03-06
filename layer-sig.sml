(* This is a functor that creates a structure
   matching the PLAYER signature, which is the
   minimal player interface. This one is augmented
   with a bunch of useful information, such as
   the current status of the playing field. *)

signature GAMESTATE =
sig

  (* Gamestate gives the state of the game from the
     perspective of a player, as the proponent. *)
  type gamestate

  (* Utilities for reading the state. *)

  (* Am I player 0? *)
  val first_player : gamestate -> bool

  (* ... *)
  val mystats : gamestate -> LTG.stats
  val theirstats : gamestate -> LTG.stats

  (* WARNING: It is unsafe to modify the returned copies. *)
  val myside : gamestate -> LTG.side
  val theirside : gamestate -> LTG.side

  val time : gamestate -> int
  
  (* Best current practices scoring function. Gives the value of the
     opponent's slot by index as a real number (larger is better)
     based on cards played, damage done, etc. If the slot is dead,
     then the result will always be negative.
     
     Might change at any point to reflect new heuristics. *)
  val scoreopponentslot : gamestate -> int -> real

  (* Same, bug give a function that computes the distance of any
     given index, in turns. *)
  val scoreopponentslot_withdistance : gamestate -> (int -> int) -> int -> real

  (* Old versions of scoring function. *)
  (* val scoreopponentslot_sniper : gamestate -> int -> real *)

  (* Prints out all stats, for debugging. 
     This should not be called in submissions, just for debugging! *)
  val printstats : LTG.stats -> unit

  
  (* These are used internally to update and create
     states. You probably shouldn't call them from
     within your player unless you are on expert mode. *)
  (* true if I am the first player. *)
  val initialstate : bool -> gamestate
  val my_turn : gamestate -> LTG.turn -> unit
  val their_turn : gamestate -> LTG.turn -> unit

end

(* Argument to layer functor. *)
signature LAYER =
sig

  (* Argument structure should have
     structure GS = GameState *)
  structure GS : GAMESTATE
  

  (* Keep all your state internally in your structure.
     There is no type arg. *)

  (* Initialize. This is always called on the initial state. *)
  val init : GS.gamestate -> unit

  val taketurn : GS.gamestate -> LTG.turn

end

(* Argument to coro-layer functor. *)
signature CORO_LAYER =
sig
  structure GS : GAMESTATE

  datatype return = RETURN of (GS.gamestate * (LTG.turn -> return))
  val algorithm : (GS.gamestate * (LTG.turn -> return)) -> 'a
end

(* In your player-strategy.sml file, do
   
   structure Player = LayerFn(Me)

   after declaring Me :> LAYER, or
   
   structure Player = LayerFn(CoroLayerFn(Me))
   
   after declaring Me :> CORO_LAYER. *)
