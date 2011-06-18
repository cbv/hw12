
(* current story: broken; attacks opponent's slot 0 once and zombies it. Working on making the zombie do something. *)

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

  (* cost *)
  (*      2 *) fun rap n = [L K n, L S n] (* reverse apply *)
  (* 3m + 4 *) fun apply_slot_to_slot n m = (rap n) @ [R n Get] @ (rep m ((rap n) @ [R n Succ])) @ [R n Z]
  (* 3m + 1 *) fun apply_slot_to_int n m = (rap n) @ [R n Succ] @ (rep (m-1) ((rap n) @ [R n Succ])) @ [R n Z] (* Min m is 1! *)
  (*      1 *) fun dbl n = [L Dbl n]
  (*      1 *) fun succ n = [L Su n]
  (*      1 *) fun fastload n card = [R n card] (* n must hold ID! *)
  (*      1 *) fun fastzero n = fastload n Z (* n must hold ID! *)
  (*      1 *) fun apply a b = [R a b]
  (*     15 *) fun twofivefive n = succ n @ (rep 7 ((dbl n) @ (succ n))) (* n must hold 0! *)

  val strategy = ref (

                 (* build our zombie function in slot 3 *)
                 (* zombies are applied to ID! *)
                 (* We want: Attack 0 1 8192 which will heal 0 and 1 *)
		 (* We will implement this as: Attack 0 1 (Copy 0) *)
                 fastload 3 
                 fastzero 2 @
	         twofivefive 2 @ (* slot 2 holds 255 *)

		 fastzero 0 @
                 succ 0 @ (* slot 0 holds 1 *)

                 fastload 1 Attack @
		 apply_slot_to_int 1 1 @
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
