(* Here live small strategies that have been hand coded for efficiency. *)
structure Macros =
struct
  fun curry2 f x y = f(x, y)
  fun rep n x = List.concat (List.tabulate (n, fn _ => x))

  open LTG

  val L = curry2 LTG.LeftApply
  val R = curry2 LTG.RightApply

  val Z = Zero
  val Su = Succ

  (* The comments on the left of the macros are the cost in "cycles" (turns.) *)
  (* The fast* macros expect to be applied to a slot that holds the identity. If needed someone could write slow versions that do a 'put' first. *)


  (* rap is 'reverse apply'. If a slot holds some function f, and you play rap on that slot followed by some card g, the result wil be: \x. f (g x) . (If you then play rap followed by some card h, you will have \x . f (g (h x)) and so forth.) *)
  (*      2 *) fun rap n = [L K n, L S n] 

  (* apply slot n to slot m, and store the result back in slot n. *)
  (* 3m + 4 *) fun apply_slot_to_slot n m = (rap n) @ [R n Get] @ (rep m ((rap n) @ [R n Succ])) @ [R n Z]

  (* apply slot n to the integer m, and store the result back in slot n. This needs to be optimized to use buldnum instead of just successor. *)
  (* 3m + 1 *) fun apply_slot_to_int n 0 = [R n Z]
                 | apply_slot_to_int n m = (rap n) @ [R n Succ] @ (rep (m-1) ((rap n) @ [R n Succ])) @ [R n Z]

  (*      1 *) fun dbl n = [L Dbl n]
  (*      1 *) fun succ n = [L Su n]
  (*      1 *) fun fastload n card = [R n card] (* n must hold ID! *)
  (*      1 *) fun fastzero n = fastload n Z (* n must hold ID! *)
  (*      1 *) fun apply a b = [R a b]

  (* must start with slot containing zero! *)
  (* cost is number of bits in n, plus number of 1 bits in n, minus one*)
  fun buildnum slot 0 = []
    | buildnum slot n = if (n mod 2 = 1) then (buildnum slot (n - 1)) @ (succ slot)
                                         else (buildnum slot (n div 2)) @ (dbl slot)

  (* cost is buildnum slot n,  + 1 *)
  fun fastnum slot n = (fastzero slot) @ (buildnum slot n)

  (* Shoots the slot 'enemy' twice, using the slots 'mine1' and 'mine2'. Note that this uses the backwards numbering for 'enemy' that the attack card uses; i.e. pass enemy=255 to shoot what the enemy calls slot 0. Sorry. *)
  (* This was initially hand-coded with mine1 and mine2 being 0 and 1. The numbers you pick will get inlined expensively, so low numbers are better. *)
  fun doubleshot enemy mine1 mine2 = (
                 fastnum 2 enemy @ (* load enemy slot number into our slot 2 *)
                 fastnum 0 8192 @ (* slot 0 holds 8192 (amt. of damage) *)

                 fastload 1 Attack @
		 apply_slot_to_int 1 mine1 @
		 apply_slot_to_slot 1 2 @
		 apply_slot_to_slot 1 0 @ (* executes Attack mine1 enemy 8192 *)

		 fastload 1 Attack @
		 apply_slot_to_int 1 mine2 @
		 apply_slot_to_slot 1 2 @
		 apply_slot_to_slot 1 0 @ (* executes Attack mine2 enemy 8192 *)

		 fastload 1 Zombie @
		 apply_slot_to_slot 1 2 @
		 apply_slot_to_int 1 0 (* executes Zombie enemy 0 *)
  )

end
