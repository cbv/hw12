(* Kind of like EMIT_PROGRAM *)
signature UNRAVEL_PROGRAM =
sig

  (* A datatype of maybe-not-straightline progams *)
  datatype todo =
	   List of Macros.turn list * todo
	 | Move of (Kompiler.src * int) * todo
	 | Call of int * todo
	 | DFn of DOS.dos -> todo
	 | Fn of GameState.gamestate -> todo
	 | Nil

  val @@ : (Kompiler.src * int) * todo -> todo
  val @@> : int * todo -> todo
  val @@>> : Macros.turn list * todo -> todo

  (* Create the dominator. Must be spawned by the caller. 
     You should own all of the slots that this 
     modifies, since the child dominator will access them. *)
  val unravel : todo -> DOS.dominator

end
