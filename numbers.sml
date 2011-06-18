(* Utilities for numbers in LTG land. *)

structure Numbers =
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

  (* XXX cost to build in binary *)

end
