structure DOS :> DOS =
struct

  structure GS = GameState

  exception DOS of string

  datatype dosturn =
(*      ReserveSlot of  *)
      Turn of LTG.turn
    (* If you can't take a turn this round, just return Can'tRun.
       The operating system will take a turn for some other dominator,
       internal operation, or if none can run, idle. *)
    | Can'tRun

  (* The argument that I give to dominators.
     Some internal state is just global. *)
  datatype dos = D of { gs : GS.gamestate }
  type dominator = { taketurn : dos -> dosturn }

  val reserved = Array.array (256, false)

  fun gamestate (D { gs, ... }) = gs

  (* PERF *)
  exception Return of int
  fun reserve_slot dos =
    let 
        val (_, vitality) = GS.myside (gamestate dos)
    in
        Util.for 0 255
        (fn i => if Array.sub (reserved, i) orelse
                    (* Maybe should also prefer slots that have higher
                       health, if we don't care about addressability? *)
                    Array.sub (vitality, i) <= 0
                 then ()
                 else (Array.update (reserved, i, true);
                       raise Return i));
        NONE
    end handle Return i => SOME i

  val reserve_addressable_slot = reserve_slot

  fun release_slot _ i =
    let in
        if Array.sub (reserved, i)
        then Array.update (reserved, i, false)
        else raise DOS ("Released unreserved slot " ^ Int.toString i)
    end

  fun makelayer (doms : (real * dominator) list) =
    let
       (* Not using priorities yet. *)
       val queue = ref (map #2 doms)

       (* TODO: everybody in queue gets to see new state. *)

       fun dos_init _ = ()
       fun dos_taketurn (gs : GS.gamestate) =
         let val dos = D { gs = gs }

             fun dosomething _ nil =
                     (* Idle!! Queue doesn't change
                        because we skipped everything. *)
                     LTG.LeftApply (LTG.I, 0)
               | dosomething skipped ((h as { taketurn }) :: t) = 
                 (case taketurn dos of
                      Can'tRun => dosomething (h :: skipped) t
                    | Turn turn =>
                       (* Only move the one that took the move
                          to the tail. *)
                       let in
                           queue := rev skipped @ t @ [h];
                           turn
                       end)
         in
             dosomething nil (!queue)
         end
    in
       (dos_init, dos_taketurn)
    end

end
