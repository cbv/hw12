(* XXX This is not a well-behaved dominator yet; it clobbers some small-numbered slots without reserving them *)

structure Gorillas :> DOMINATOR =
struct
  structure GS = GameState
  structure K = Kompiler
  datatype src = datatype K.src
  datatype card = datatype Card.card
  datatype todo = datatype UnravelProgram.todo
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
  val slosh2 = 2

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
  
  fun dprint x = (if !debugs > 0 then (debugs := !debugs - 1; eprint (Int.toString (!counter) ^ " " ^ x ^ "\n")) else ())
 	  
  fun also [] x = x
    | also (h::tl) x = h @@ also tl x

  val helpSmall = install (help ` Int battery ` Int battery ` get (Int small)) 
  val slosher = install (put (help ` Int battery ` Int battery ` get (Int big)) (help ` Int battery ` Int battery ` get (Int big)))
  val ass = install (attack ` Int battery ` (get (Int loc)) ` get (Int med))
	       
  fun charge gs = let
      val (_, vits) = GS.myside gs
      val v = Array.sub (vits , battery)
      val attacker = ass (* if sweep then sweeper else ass *)
      val plan = attacker @@> [L(Succ, loc)] @@>> Fn charge
  in
      if v < 10000
      then (dprint ("Rebooting!"); (revive ` Int 0, 1) 
				       @@ (help ` Int slosh ` Int battery ` Int 5000, 1) 
				       @@ (help ` Int slosh2 ` Int battery ` Int 5000, 1) @@ Fn main)
      else if (v <= cThresh) 
      then helpSmall @@> Fn charge
      else if (v < aThresh)
      then slosher @@> Fn charge
      else (dprint("Attacking"); plan)
  end
 and main gs = (Int 0, loc) @@ (Int 11112, med) @@ (Int cThresh, big) @@ (Int 9999, small) @@ also (!codes) (Fn charge)
 and prefix gs = Macros.doubleshot 255 2 3 @@>> Fn main

fun create () = UnravelProgram.unravel (Fn prefix)
end
