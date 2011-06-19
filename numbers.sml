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

  fun unslotify hts = 
          map (fn LTG.LeftApply (c, _) => LTG.HLeftApply c
                | LTG.RightApply (_, c) => LTG.HRightApply c) hts

  (* Precompute how to build numbers naively. *)
  val compiled_number_table = 
      Array.tabulate (256, fn n => unslotify (Kompiler.compile (Kompiler.Int n) 0))

  (* Naive cost is the cost of of building a number from scratch.  Corresponds
     to the length of (compile VInt n _). *)
  val naive_cost_table = 
      Array.tabulate (256, fn n => length (Array.sub (compiled_number_table, n)))
  fun naive_cost n = Array.sub (naive_cost_table, n)

  val addressability =
      let
        val l = Array.foldli (fn (i, cost, acc) => (i, cost) :: acc) nil naive_cost_table
        val l = ListUtil.stablesort (ListUtil.bysecond Int.compare) l
      in
        Array.fromList (map #1 l)
      end

  val max_naive_cost = Array.foldl (fn (x, y) => if x > y then x else y) 0 naive_cost_table

  fun convert_from { given : int, desired : int} = 
      let
        fun from given = 
            if given > desired then NONE
            else if given = desired then SOME []
            else 
              (case (from (given * 2), from (given + 1)) of
                 (SOME hts, NONE) => SOME ((LTG.HLeftApply LTG.Dbl) :: hts)
               | (NONE, SOME hts) => SOME ((LTG.HLeftApply LTG.Succ) :: hts)
               | (SOME htsdbl, SOME htssucc) =>
                 if length htssucc < length htssucc
                 then SOME ((LTG.HLeftApply LTG.Succ) :: htssucc)
                 else SOME ((LTG.HLeftApply LTG.Dbl) :: htsdbl)
               | (NONE, NONE) => NONE)
      in
        if desired >= 256 then (* Just run the compiler. *)
          unslotify (Kompiler.compile (Kompiler.Int desired) 0)
        else (* Try to search for a good reuse first. *)
          case Memoize.memoize (Memoize.idx_tabler (fn x => x) given 256) from given of
            SOME hts => rev hts
          | NONE => Array.sub (compiled_number_table, desired)
      end

  datatype card = datatype Card.card
  datatype halfturn = datatype LTG.halfturn
  fun test () = 
      let val x = [convert_from {given = 10, desired = 4},
                   convert_from {given = 10, desired = 10},
                   convert_from {given = 10, desired = 20},
                   convert_from {given = 10, desired = 21},
                   convert_from {given = 10, desired = 32},
                   convert_from {given = 9999, desired = 8191}]
          val _ = x = [[HLeftApply Put,HRightApply Zero,HLeftApply Succ,HLeftApply Dbl,
                        HLeftApply Dbl],
                       [],
                       [HLeftApply Dbl],[HLeftApply Succ,HLeftApply Dbl],
                       [HLeftApply Succ,HLeftApply Succ,HLeftApply Succ,HLeftApply Succ,
                        HLeftApply Succ,HLeftApply Succ,HLeftApply Succ,HLeftApply Succ,
                        HLeftApply Succ,HLeftApply Succ,HLeftApply Succ,HLeftApply Succ,
                        HLeftApply Dbl],
                       [HLeftApply Put,HRightApply Zero,HLeftApply Succ,HLeftApply Dbl,
                        HLeftApply Succ,HLeftApply Dbl,HLeftApply Succ,HLeftApply Dbl,
                        HLeftApply Succ,HLeftApply Dbl,HLeftApply Succ,HLeftApply Dbl,
                        HLeftApply Succ,HLeftApply Dbl,HLeftApply Succ,HLeftApply Dbl,
                        HLeftApply Succ,HLeftApply Dbl,HLeftApply Succ,HLeftApply Dbl,
                        HLeftApply Succ,HLeftApply Dbl,HLeftApply Succ,HLeftApply Dbl,
                        HLeftApply Succ,HLeftApply Dbl,HLeftApply Succ]]
                   orelse raise Kompiler.Kompiler "You broke Numbers.convert_from."
      in
        x
      end

end
