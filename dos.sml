structure DOS :> DOS =
struct

  structure GS = GameState

  exception DOS of string

  (* The argument that I give to dominators.
     Some internal state is just global. *)
  datatype dos = D of { gs : GS.gamestate }
  type dominator = { taketurn : dos -> LTG.turn }

  val reserved = Array.array (256, false)

  fun gamestate (D { gs, ... }) = gs

  (* PERF *)
  exception Return of int
  fun reserve_slot _ =
    let in
        Util.for 0 255
        (fn i => if Array.sub (reserved, i)
                 then ()
                 else (Array.update (reserved, i, true);
                       raise Return i));
        raise DOS "no more slots"
    end handle Return i => i

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

       fun dos_init _ = ()
       fun dos_taketurn (gs : GS.gamestate) =
         let val dos = D { gs = gs }
         in
             case !queue of
                 nil =>
                     let in
                         eprint ("DOS: Out of tasks to run. Press the " ^
                                 "power button to restart.\n");
                         LTG.LeftApply (LTG.I, 0)
                     end
               | (h as { taketurn }) :: t =>
                   let in
                       queue := t @ [h];
                       taketurn dos
                   end
         end
    in
       (dos_init, dos_taketurn)
    end

end
