(* Kill's itself after one iteration. *)
structure UnravelProgram2 : UNRAVEL_PROGRAM =
struct

 structure GS = GameState
 structure K = Kompiler
 datatype src = datatype K.src
 datatype card = datatype Card.card
 open CardHelp
      
 val L = LTG.LeftApply
 val R = LTG.RightApply  

 infix 1 `
 fun a ` b = a b

 datatype todo =
	   List of Macros.turn list * todo
	 | Move of (src * int) * todo
	 | Call of int * todo
	 | DFn of DOS.dos -> todo
	 | Fn of GS.gamestate -> todo
	 | Nil

  val @@ = Move
  val @@> = Call
  val @@>> = List

  fun unravel t = let
      val script = ref t
      fun preview _ = ()
      fun taketurn dos = let
	  val gs = DOS.gamestate dos
	  fun f Nil = (DOS.kill (DOS.getpid dos); L(Card.I, 0))
	    | f (List(t::ts, next)) = (script := List (ts, next); t)
	    | f (List([], next)) = go next
	    | f (Move((p,n), next)) = go ` List (K.compile p n, next)
	    | f (Fn h) = go ` h gs
	    | f (DFn h) = go ` h dos
	    | f (Call(i, next)) = (script := next; R (i, Zero))
	  and go x = (script := x; f (!script))
      in
	  DOS.Turn (f (!script)) (* TODO: Avoid executing dead cells *)
      end

  in
      {preview=preview, taketurn=taketurn}
  end

end
