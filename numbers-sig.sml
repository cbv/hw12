signature NUMBERS =
sig

  val next_lowest_power_of_two : int -> int

  (* The sequence of half-turns that should be applied to a cell that
     currently contains |given| to change the contents of the cell to
     |desired|.  At worst, it will just return the cards that start from
     scratch. *)
  val convert_from : { given : int, desired : int} -> LTG.halfturn list

  (* testing only *)
  val test : unit -> LTG.halfturn list list

end
