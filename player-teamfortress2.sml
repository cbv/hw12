(* This strategy uses the tried and true Team Fortress 2
   class composition, which is known to maximize teamwork.
   Not all classes are implemented. :) *)
structure PlayerTF2 :> LAYER =
struct
  structure GS = GameState

  (* [ ] Scout         - (blitzkrieg)
     [ ] Soldier       - (suicide-bombs enemy?)
     [ ] Flametrooper
     [ ] Demoman       - (creates demons)
     [ ] Heavy         - (defends blitzes?)
     [ ] Engineer      - (builds useful numbers?)
     [x] Medic         - Selects targets and heals them
     [x] Sniper        - Selects high-value enemies and kills them
     [ ] Spy           - (copies useful programs? make zombies?)
     *)
  val (init, taketurn) =
      DOS.makelayer [(1.0, Sniper.create ()),
                     (1.0, Medic.create ())]
end

structure Player = LayerFn(PlayerTF2)

