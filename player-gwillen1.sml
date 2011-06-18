
structure Gwillen1 :> LAYER =
struct
  structure GS = GameState

  (* This one doesn't pay any attention to the
     game state. *)
  fun init _ = ()

  fun curry2 f x y = f(x, y)
  fun rep n x = List.concat (List.tabulate (n, fn _ => x))

  open LTG

  val L = curry2 LTG.LeftApply
  val R = curry2 LTG.RightApply

  val Z = Zero
  val Su = Succ

  fun rap n = [L K n, L S n] (* reverse apply *)
  (*val apply_to_slot_0 n = (rap n) @ [R n Get, R n Z] (* apply cell to "get 0", i.e. contents of cell zero *)*)
  fun apply_slot_to_slot n m = (rap n) @ [R n Get] @ (rep m ((rap n) @ [R n Succ])) @ [R n Z]
  fun dbl n = [L Dbl n]
  fun succ n = [L Su n]
  fun fastload n card = [R n card] (* n must hold ID! *)
  fun fastzero n = fastload n Z (* n must hold ID! *)
  fun apply a b = [R a b]
  fun twofivefive n = succ n @ (rep 7 ((dbl n) @ (succ n))) (* n must hold 0! *)

  val strategy = ref (

                 fastzero 2 @
	         twofivefive 2 @ (* slot 2 holds 255 *)

		 fastzero 0 @
                 succ 0 @ (* slot 0 holds 1 *)

                 fastload 1 Attack @
		 apply_slot_to_slot 1 0 @
		 apply_slot_to_slot 1 2 @ (* slot 1 holds (Attack 1 255 ...) *)
		 rep 13 (dbl 0) @ (* slot 0 holds 8192 *)
		 apply_slot_to_slot 1 0 @ (* executes Attack 1 255 8192 *)

		 fastload 1 Attack @
		 apply 1 Z @
		 apply_slot_to_slot 1 2 @
		 apply_slot_to_slot 1 0 @ (* executes Attack 0 255 8192 *)

		 fastload 1 Zombie @
		 apply_slot_to_slot 1 2 @
		 apply 1 Z (* executes Zombie 255 0 *)
  )

  fun taketurn _ = case (!strategy) of nil => L Z 0
    | today::future => let 
      val _ = strategy := future
    in
      today
    end

end

structure Player = LayerFn(Gwillen1)
