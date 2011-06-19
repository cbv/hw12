(* Test of the number generator.
*)
structure PlayerNumberGeneratorTest :> LAYER =
struct
  structure GS = GameState
  datatype status = datatype NumberGenerator.status

  fun preview _ = ()

  val queue = ref [4, 4, 5, 10, 12, 20]
  val status = ref (ref (Done ~1)) (* ~1 means we haven't asked for anything yet. *)

  fun taketurn dos = 
      case (!queue, !(!status)) of 
        (nil, _) => (* Really done. *)
        DOS.Can'tRun

      | (i :: rest, Done ~1) => (* Ask for a new one. *)
        (status := NumberGenerator.generate dos i;
         queue := rest;
         DOS.Can'tRun)

      | (_, NotDone) => (* Waiting... *)
        DOS.Can'tRun

      | (_, Done i) => (* Got one *)
        (eprint ("Got result in slot " ^ Int.toString i ^ "\n");
         DOS.release_slot dos i;
         status := ref (Done ~1);
         taketurn dos)

  val (init, taketurn) = 
      DOS.makelayer [("Harness", 1.0, {preview = preview, taketurn = taketurn}),
                     ("NG", 1.0, NumberGenerator.create ())]
end

structure Player = LayerFn(PlayerNumberGeneratorTest)
