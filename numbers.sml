(* Utilities for numbers in LTG land. *)

structure Numbers :> NUMBERS =
struct

  fun next_lowest_power_of_two n =
      let
          fun find nlp =
              if nlp * 2 < n
              then find (nlp * 2)
              else nlp
      in
          find 1
      end

  (* Precompute how to build numbers naively. *)
  val compiled_number_table = 
      Array.tabulate (256, fn n =>
          map (fn LTG.LeftApply (c, _) => LTG.HLeftApply c
                | LTG.RightApply (_, c) => LTG.HRightApply c)
          (Kompiler.compile (Kompiler.Int n) 0))

  (* Naive cost is the cost of of building a number from scratch.  Corresponds
     to the length of (compile VInt n _). *)
  val naive_cost_table = 
      Array.tabulate (256, fn n => length (Array.sub (compiled_number_table, n)))
  fun naive_cost n = Array.sub (naive_cost_table, n)

  (* The sequence of cards that should be left applied to a cell that
     currently contains |given| to change the contents of the cell to
     |desired|.  At worst, it will just return the cards that start from
     scratch. *)
  fun convert_from { given : int, desired : int} = 
      let
        fun from given desired = 
            if given > desired then NONE
            else if given = desired then SOME []
            else 
              (case (from (given + 1) desired, from (given * 2) desired) of
                   (SOME hts, NONE) => SOME ((LTG.HLeftApply LTG.Succ) :: hts)
                 | (NONE, SOME hts) => SOME ((LTG.HLeftApply LTG.Dbl) :: hts)
                 | (SOME htssucc, SOME htsdbl) =>
                   if length htssucc < length htssucc
                   then SOME ((LTG.HLeftApply LTG.Succ) :: htssucc)
                   else SOME ((LTG.HLeftApply LTG.Dbl) :: htsdbl)
                 | (NONE, NONE) => NONE)
      in
        case from given desired of
          SOME hts => rev hts
        | NONE => Array.sub (compiled_number_table, desired)
      end

  fun test () = 
      [convert_from {given = 10, desired = 4},
        convert_from {given = 10, desired = 10},
        convert_from {given = 10, desired = 20},
        convert_from {given = 10, desired = 21},
        convert_from {given = 10, desired = 32}]

end
