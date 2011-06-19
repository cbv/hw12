(* XXX This is not a perfectly well-behaved dominator yet; it hardcodes vitality access to some cells *)

structure Gorillas :> DOMINATOR =
struct
  structure GS = GameState
  structure K = Kompiler
  datatype src = datatype K.src
  datatype card = datatype Card.card
  datatype todo = datatype UnravelProgram.todo
  open CardHelp

  (* Annotate messages and reduce spew *)
  val lastmsg = ref ""
  val eprint =
      fn s => if s = !lastmsg
              then ()
              else (eprint ("[GORILLAS] " ^ s ^ "\n"); lastmsg := s)

  val L = LTG.LeftApply
  val R = LTG.RightApply  

  fun init gs = ()

  infix 9 --
  val op -- = Apply

  val $ = Var
  fun \ x exp = Lambda (x, exp)
  infix 1 `
  fun a ` b = a b

  infixr 2 @@
  val op @@ = Move

  infixr 2 @@>
  fun op @@> (x, y) = Call(!x, y)

  infixr 2 @@>>
  val op @@>> = List

  val counter = ref 0
  val debugs = ref 50

  fun create () = let
      (* cells accessed for vitality only *)
      val battery = 0
      val slosh = 1
      val slosh2 = 3
      val rush1 = 2
      val rush2 = 4

      (* allocated cells *)
      val warm = ref false
      val small = ref 0
      val big = ref 0
      val med = ref 0
      val loc = ref 0
      val helpSmall = ref 0
      val slosher = ref 0
      val ass = ref 0

      val cThresh = 47000
      val aThresh = 58200

      val codes = ref []
		      
      fun dprint gs x = (if !debugs > 0 then (debugs := !debugs - 1; eprint (Int.toString (GS.time gs div 2) ^ " " ^ x ^ "\n")) else ())
 		     
      fun also [] x = x
	| also (h::tl) x = h @@ also tl x

      fun getr n  = get (Int (!n))
      fun setr n m  = (Int m, !n)

      fun andthen (x, y) = (\ "_" ` y) -- x
      (* fun andthen (x, y) = put x y *)
      infixr 0 andthen

      fun boot dos = let
	  fun slot() = (* HAX *) let val c = !counter in counter := c + 1; c end
	  fun install (s, x) = let 
	      val n = slot()
(*	      val _ = eprint (s ^ ":\n")
	      val _ = eprint ( (K.kil2str(K.src2kil x)) ^ "\n")
	      val _ = eprint ( Int.toString(length(K.compile (K.rrs_ref x n) n)) ^ "\n")
	      val _ = eprint ( Int.toString(length(K.compile (\ "_" ` x) n)) ^ "\n") *)
	      val _ = codes := (K.rrs_ref x n, n) :: !codes
	  in
	      n
	  end
      in
	  big := slot();
	  small := slot();
	  med := slot();
	  loc := slot();
	  helpSmall := install ("hs", help ` Int battery ` Int battery ` getr small);
	  ass := install ("ass", attack ` Int battery ` getr loc ` getr med);
	  slosher := install ("slosher", 
			      put (help ` Int battery ` Int battery ` getr big)
				  (help ` Int battery ` Int battery ` getr big));

	  warm := true;
	  ()
      end

      fun charge gs = let
	  val _ = dprint gs ("Charging")
	  val (vals, vits) = GS.myside gs
	  val (_, ovits) = GS.theirside gs
	  val v = Array.sub (vits , battery)
	  val attacker = ass 
	  fun decide gs = let

	      val moveOn = (case Array.sub (vals , !loc) of
				LTG.VInt n => Array.sub (ovits, 255 - n) <= 0
			      | _ => true) handle Subscript => true
	  in
	      if moveOn then [L(Succ, !loc)] @@>> Fn charge else Fn charge
	  end
	  val plan = attacker @@> Fn decide
      in
	  if v < 10000
	  then (dprint gs ("Rebooting!"); (revive ` Int 0, 1) 
			            @@ (help ` Int slosh ` Int battery ` Int 5000, 1) 
				    @@ (help ` Int slosh2 ` Int battery ` Int 5000, 1)
				    @@ Fn main)
	  else if (v <= cThresh) 
	  then helpSmall @@> Fn charge
	  else if (v < aThresh)
	  then slosher @@> Fn charge
	  else (dprint gs ("Attacking"); plan)
      end
      and main gs = setr loc 0
		 @@ setr med 11112 
		 @@ setr big cThresh 
		 @@ setr small 9999 
		 @@ also (!codes) (Fn charge)
      and rush gs = Macros.doubleshot 255 rush1 rush2 @@>> Fn main
      and loader dos = ((if not (!warm) then boot(dos) else ()); Fn rush)

  in
      UnravelProgram.unravel (DFn loader)
  end (* fun create *)

end
