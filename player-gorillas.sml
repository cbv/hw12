(* Gorillas is a video game first distributed with MS-DOS 5 and
published in 1991 by IBM corporation. *)

structure P :> LAYER =
struct
  structure GS = GameState
  val (init, taketurn) = DOS.makelayer [(1.0, Gorillas.create ())]    
end

structure Player = LayerFn(P)

