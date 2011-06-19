(* Here live small strategies that have been hand coded for efficiency. *)
structure Macros =
struct
  open LTG
  open Kompiler

  infix 9 --
  val op -- = Apply
  val $ = Var
  fun \ x exp = Lambda (x, exp)
  infixr 1 `
  fun a ` b = a b

  fun curry2 f x y = f(x, y)
  fun rep n x = List.concat (List.tabulate (n, fn _ => x))


  val L = curry2 LTG.LeftApply
  val R = curry2 LTG.RightApply

  val Z = Zero
  val Su = Succ

  (* The comments on the left of the macros are the cost in "cycles" (turns.) *)
  (* The fast* macros expect to be applied to a slot that holds the identity. The slow* versions clear first. *)

  (*      1 *) fun clear slot = [L Put slot]

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
  (*      2 *) fun slowload n card = (clear n) @ (fastload n card)
  (*      1 *) fun fastzero n = fastload n Z (* n must hold ID! *)
  (*      2 *) fun slowzero n = slowload n Z
  (*      1 *) fun apply a b = [R a b]

  (* must start with slot containing zero! *)
  (* cost is number of bits in n, plus number of 1 bits in n, minus one*)
  (* Call this "blog n" or {n}*)
  fun buildnum slot 0 = []
    | buildnum slot n = if (n mod 2 = 1) then (buildnum slot (n - 1)) @ (succ slot)
                                         else (buildnum slot (n div 2)) @ (dbl slot)

  (* cost is {n} + 1 *)
  fun fastnum slot n = (fastzero slot) @ (buildnum slot n)
  (* cost is {n} + 2 *)
  fun slownum slot n = (slowzero slot) @ (buildnum slot n)
 

  (* 3 *) fun rcomp n card = rap n @ [R n card]

  (* f[n] = rrs_ref(f[m]), expects slot n to be Id *)
  (* cost is 13 + 3m + 3n *)
  fun fast_rr n m = fastload n Get @ [ L K n, L S n] @ rcomp n K @ apply_slot_to_int n n @ [L K n, L S n] @ apply_slot_to_slot n m

  datatype num = IMM of int | REG of int

  (* Shoots the slot 'enemy' twice, using the slots 'mine1' and 'mine2'. Note that this uses the backwards numbering for 'enemy' that the attack card uses; i.e. pass enemy=255 to shoot what the enemy calls slot 0. Sorry. *)
  (* This was initially hand-coded with mine1 and mine2 being 0 and 1. The numbers you pick will get inlined expensively, so low numbers are better. *)

  fun doubleshot (enemy : int) (mine1 : int) (mine2 : int) : LTG.turn list =
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

  fun doubleshot_gwillen_don't_break_other_people's_code (enemy : num) (mine1 : int) (mine2 : int) (scratch : int) (scratch2 : int) : LTG.turn list =
    let
      val (load_enemy, enemy_cell) = case enemy of
          REG n => ([], n)
        | IMM n => (fastnum scratch n, scratch)
    in
      load_enemy @
      fastnum 0 8192 @ (* slot 0 holds 8192 (amt. of damage) *)

      fastload scratch2 Attack @
      apply_slot_to_int scratch2 mine1 @
      apply_slot_to_slot scratch2 enemy_cell @
      apply_slot_to_slot scratch2 0 @ (* executes Attack minescratch2 enemy 8292 *)

      fastload scratch2 Attack @
      apply_slot_to_int scratch2 mine2 @
      apply_slot_to_slot scratch2 enemy_cell @
      apply_slot_to_slot scratch2 0 @ (* executes Attack mine2 enemy 8292 *)

      fastload scratch2 Zombie @
      apply_slot_to_slot scratch2 enemy_cell @
      apply_slot_to_int scratch2 0 @ (* executes Zombie enemy 0 *)

      clear scratch @
      clear 0
    end

  (* like the above, with enemy = 0 and mine = 0 and 1 *)
  (* 35 cycles *)
  fun fastest_doubleshot () : LTG.turn list =
      fastnum 0 8192 @ (* slot 0 holds 8192 (amt. of damage) *)

      fastload 1 Attack @
      apply_slot_to_int 1 0 @
      apply_slot_to_int 1 0 @
      apply_slot_to_slot 1 0 @ (* executes Attack mine1 enemy 8192 *)

      fastnum 1 1 @
      [L Attack 1] @
      apply 1 Z @
      apply_slot_to_slot 1 0 

  (* best used with 0 0 1 0 *)
  fun fast_doubleshot (scratch : int) (dmg_slot : int) (source1 : int) (source2 : int) (target : int) : LTG.turn list =
      fastload scratch Attack @
      apply_slot_to_int scratch source1 @
      apply_slot_to_int scratch target @
      apply_slot_to_slot scratch dmg_slot @ (* executes Attack mine1 enemy 8192 *)

      fastnum scratch source2 @
      [L Attack scratch] @
      apply_slot_to_int scratch target  @
      apply_slot_to_slot scratch dmg_slot
(*
      fastload 1 Zombie @
      apply_slot_to_int 1 0 @
      apply_slot_to_int 1 0 (* executes Zombie enemy 0 *)
*)

 
end
