(* Player, please! *)

signature PLAYER = sig
   (* If your implementation is imperative, this can just be unit *)
   type state

   (* The first player gets called with init NONE, and the second player 
    * gets called with init SOME (other-player's-first-move) *)
   val init: LTG.turn option -> LTG.turn * state

   (* In each round, we get the competetitor's move and our previous state
    * and have to give a new move and a new state *)
   val round: LTG.turn * state -> LTG.turn * state 
end
