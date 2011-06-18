structure EmitProgram :> EMIT_PROGRAM =
struct
  structure GS = GameState

  datatype status =
      Progress of real
    | Paused of int
    | Done

  fun emit turns =
      let 
          val icr = 1.0 / real (length turns + 1)
          val progress = ref 0.0
          val status = ref (Progress 0.0)

          fun preview _ = ()
              
          val turnsleft = ref turns
          fun taketurn dos =
            (case !turnsleft of
                 nil => (status := Done;
                         DOS.kill (DOS.getpid dos);
                         DOS.Can'tRun)
               | (t :: turns) =>
                  let val gs = DOS.gamestate dos
                      val slot = case t of
                          LTG.LeftApply (_, i) => i
                        | LTG.RightApply (i, _) => i
                  in
                      if LTG.slotisdead (GS.myside gs) slot
                      then (status := Paused slot;
                            (* Someone might still revive this slot,
                               but we'll be blocked until then. *)
                            DOS.Can'tRun)
                      else (progress := !progress + icr;
                            status := Progress (!progress);
                            turnsleft := turns;
                            DOS.Turn t)
                  end)
      in
          (status, { preview = preview, taketurn = taketurn })
      end

  fun emitspawn dos turns = 
      let val (status, dom) = emit turns
          val pid = DOS.spawn (SOME (DOS.getpid dos))
              (DOS.getpriority dos, dom)
      in
          (status, pid)
      end

end
