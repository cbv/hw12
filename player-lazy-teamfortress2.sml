(* This strategy uses the tried and true Team Fortress 2
   class composition, which is known to maximize teamwork.
   Not all classes are implemented. :) *)
structure PlayerTF2 :> LAYER =
struct
  structure GS = GameState

  (* [x] Scout         - (blitzkrieg offense / defense)
     [ ] Soldier       - (suicide-bombs enemy?)
     [ ] Flametrooper  = (top secret)
     [ ] Demoman       - (creates demons)
     [ ] Heavy         - (defends blitzes?)
     [ ] Engineer      - (builds useful numbers?)
     [x] Medic         - Selects targets and heals them
     [x] Sniper        - Selects high-value enemies and kills them
     [ ] Spy           - (make zombies?)
     *)
  val (init, taketurn) =
      DOS.makelayer [("Scout", 1000.0, Scout.create ()),
                     ("Sniper", 1.0, Sniper.create ()),
                     ("LazyMedic", 1.0, LazyMedic.create ())]
end

structure Player = LayerFn(PlayerTF2)

