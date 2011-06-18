(* Example player that uses DOS 3.1. 
   Threads can be composed at any ratio and priority;
   you should make your own player file instead of
   modifying this one in place.
*)
structure PlayerDOS31Demo :> LAYER =
struct
  structure GS = GameState

  (* [ ] Scout
     [ ] Soldier
     [ ] Flametrooper
     [ ] Demoman
     [ ] Heavy
     [ ] Engineer
     [ ] Medic
     [ ] Sniper
     [ ] Spy *)
  val (init, taketurn) =
      DOS.makelayer [(1.0, Ripens.create ()),
                     (1.0, Medic.create ())]
end

structure Player = LayerFn(PlayerDOS31Demo)

