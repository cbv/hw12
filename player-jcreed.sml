structure Jcreed :> LAYER =
struct
  structure GS = GameState
  structure K = Kompiler
  datatype src = datatype K.src
  datatype card = datatype Card.card
  open CardHelp

  val L = LTG.LeftApply
  val R = LTG.RightApply  

  fun init gs = ()

  infix 9 --
  val op -- = Apply


  val $ = Var
  fun \ x exp = Lambda (x, exp)
  infix 1 `
  fun a ` b = a b

  datatype todo =
	   List of Macros.turn list * todo
	 | Move of (src * int) * todo
	 | Call of int * todo
	 | Fn of GS.gamestate -> todo
	 | Nil

  infixr 2 @@
  val op @@ = Move

  infixr 2 @@>
  val op @@> = Call

  infixr 2 @@>>
  val op @@>> = List

  val counter = ref 0
  val debugs = ref 50

  val battery = 0
  val slosh = 1

  val small = 0
  val big = 1
  val med = 2
  val loc = 3
  val cThresh = 47000
  val aThresh = 58200

  val codeseg = ref 4
  val codes = ref []
  fun install x = let 
      val n = !codeseg
      val _ = codeseg := !codeseg + 1
      val _ = codes := (K.rrs_ref x n, n) :: !codes
  in
      n
  end
  

  fun dprint x = (if !debugs > 0 then (debugs := !debugs - 1; eprint x) else ())
 
	  
  fun also [] x = x
    | also (h::tl) x = h @@ also tl x

  fun andthen (x, y) = (\ "_" ` y) -- x
  (* fun andthen (x, y) = put x y *)

  infixr 0 andthen

  val helpSmall = install (help ` Int battery ` Int battery ` get (Int small)) 
  val slosher = install (put (help ` Int battery ` Int battery ` get (Int big)) (help ` Int battery ` Int battery ` get (Int big)))
  val ass = install (attack ` Int battery ` (get (Int loc)) ` get (Int med))


 fun charge sweep gs = let
      val (_, vits) = GS.myside gs
      val v = Array.sub (vits , battery)
      val attacker = ass (* if sweep then sweeper else ass *)
      val plan = if sweep 
		    then attacker @@> [L(Succ, loc)] @@>> Fn (charge true)
		    else attacker @@> (zero, loc) @@ Fn (charge true)
  in
      if (v <= cThresh) 
      then helpSmall @@> Fn (charge sweep)
      else if (v < aThresh)
      then slosher @@> Fn (charge sweep)
      else (dprint("Attacking " ^ Int.toString (!counter) ^ "\n"); plan)
  end


  val moves =  (Int 255, loc) @@ (Int 11112, med) @@ (Int cThresh, big) @@ (Int 9999, small) @@ also (!codes) (Fn (charge false))
  val script = ref moves


  fun valtos (LTG.VInt i) = Int.toString i
    | valtos (LTG.VFn f) = let val s = LTG.ftos f in substring(s, 0, Int.min(size s - 1, 10)) end

  fun taketurn gs = let

      fun f Nil = L (I, 0)
	| f (List(t::ts, next)) = (script := List (ts, next); t)
	| f (List([], next)) = go next
	| f (Move((p,n), next)) = go ` List (K.compile p n, next)
	| f (Fn f) = go ` f gs
	| f (Call(i, next)) = (script := next; R (i, Zero))
      and go x = (script := x; f (!script))

      val (vals, vits) = GS.myside gs
      fun showscores () = 
	  if ((Array.sub(vits,0) > 45000)  orelse Array.sub(vits, 1) > 10000 )
	  then let
		  val vitsmap = List.tabulate (8, fn x => (Int.toString (Array.sub (vits,x))))
		  val valsmap = List.tabulate (8, fn x => (valtos (Array.sub (vals,x))))
		  in
		  dprint (Int.toString(!counter)^"f=["^(String.concatWith "," valsmap)^"]\n");
		  dprint (Int.toString(!counter)^"v=["^(String.concatWith "," vitsmap)^"]\n")
	      end
	  else ()
      val _ = counter := (!counter) + 1
  in
      showscores();
      f (!script)

  end
      
end

 
structure Player = LayerFn(Jcreed)
