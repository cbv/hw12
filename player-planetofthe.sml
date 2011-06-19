(* Gorillas is a video game first distributed with MS-DOS 5 and
published in 1991 by IBM corporation. *)

structure P :> LAYER =
struct
  structure GS = GameState
  val (init, taketurn) = DOS.makelayer [("Gorillas", 1000.0, Gorillas.create ()),
                                        ("Sniper", 1.0, Sniper.create ()),
                                        ("Medic", 1.0, Medic.create ())]

end

structure Player = LayerFn(P)

